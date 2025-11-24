;;; flywire.el --- Arms and eyes for automated agents -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Scott Trinh

;; Author: Scott Trinh <scott@scotttrinh.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: processes, tools, extensions
;; URL: https://github.com/scotttrinh/flywire

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Public surface for the flywire package.  This module coordinates
;; snapshots and tool execution so an external agent can see Emacs state
;; and drive it via scripted steps.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'flywire-snapshot)
(require 'flywire-action)
(require 'flywire-async)

(defgroup flywire nil
  "Arms and eyes for automated agents."
  :group 'tools
  :prefix "flywire-")

;;; Structures

(cl-defstruct flywire-env
  name
  (run #'funcall)           ; (lambda (thunk)) -> execute within this env
  (snapshot #'flywire-snapshot-get-snapshot) ; (lambda (&optional profile)) -> snapshot plist
  (enable-events #'ignore) ; (lambda (session opts)) -> install hooks/timers, optional
  (teardown #'ignore))     ; (lambda (session)) -> optional cleanup

(cl-defstruct (flywire-session
               (:constructor flywire-session--create))
  id
  env          ; flywire-env instance
  options      ; plist of configuration options
  state        ; internal state (messages, throttling, etc.)
  handlers)    ; event handlers / callbacks

(defvar flywire-session--current nil
  "Global default session.")

;;; Core Session API

(defun flywire--default-enable-events (_session _opts)
  "Default event setup: do nothing for now."
  ;; This can be expanded to replicate flywire-async behavior scoped to the session
  nil)

(defun flywire-env-default ()
  "Return a default environment representing the current Emacs session."
  (make-flywire-env
   :name "default"
   :run #'funcall
   :snapshot #'flywire-snapshot-get-snapshot
   :enable-events #'flywire--default-enable-events))

(defun flywire-session-create (&rest args)
  "Create a new session.
ARGS can include :id, :env, :snapshot-profile, :safety-policy, :events."
  (let ((env (plist-get args :env))
        (id (or (plist-get args :id) (gensym "flywire-session-"))))
    (flywire-session--create
     :id id
     :env (or env (flywire-env-default))
     :options args
     :state (list :messages nil)
     :handlers nil)))

(defun flywire-session-current ()
  "Return the global default session, creating it if necessary."
  (or flywire-session--current
      (setq flywire-session--current (flywire-session-create :id 'global))))

(defun flywire-session-on-event (session handler)
  "Register HANDLER for events on SESSION."
  (push handler (flywire-session-handlers session)))

(defun flywire-session-enable-events (session &optional opts)
  "Ask the session environment to set up any required hooks/timers."
  (let ((env (flywire-session-env session)))
    (funcall (flywire-env-enable-events env) session opts)))

(defun flywire--emit-event (session event)
  "Emit EVENT to handlers on SESSION."
  (let ((handlers (flywire-session-handlers session)))
    (dolist (h handlers)
      (funcall h event))))

;;; Execution Logic

(defun flywire--execute-step (step)
  "Execute STEP, which comes from a parsed JSON instruction.
STEP should be an alist with at least an `action` entry."
  (pcase (alist-get 'action step nil nil #'string=)
    ("type"
     (flywire-action-push-input (alist-get 'text step)))
    ("key"
     (flywire-action-simulate-keys (alist-get 'chord step)))
    ("command"
     (flywire-action-execute-tool "run_command"
                                       `((name . ,(alist-get 'name step)))))
    (_
     (error "Flywire: unknown action %S" step))))

(defun flywire--capture-messages (thunk)
  "Run THUNK and return any new text added to *Messages*."
  (let ((msg-buf (get-buffer-create "*Messages*")))
    (with-current-buffer msg-buf
      (let ((start (point-max)))
        (funcall thunk)
        (if (> (point-max) start)
            (buffer-substring-no-properties start (point-max))
          nil)))))

(defun flywire--execute-steps-with-result (session instructions &optional opts)
  "Execute INSTRUCTIONS using SESSION and OPTS.
Returns a result plist.  Captures errors and messages.
SESSION and OPTS are currently ignored."
  (ignore session opts)
  ;; Note: session is unused here for now, but will be used for message capturing later.
  (let ((step-results nil)
        (status :ok)
        (error-info nil)
        (captured-messages nil))
    (setq captured-messages
          (flywire--capture-messages
           (lambda ()
             (condition-case err
                 (dolist (step instructions)
                   (flywire--execute-step step)
                   (push `((:step-index . ,(length step-results))
                           (:action . ,step)
                           (:status . :ok))
                         step-results))
               (quit
                (setq status :cancelled)
                (setq error-info '(:type quit :message "Cancelled")))
               (error
                (setq status :error)
                (setq error-info `(:type error :message ,(error-message-string err) :data ,err)))))))
    
    (list :status status
          :steps (nreverse step-results)
          :messages captured-messages
          :error error-info)))

(defun flywire-session-exec (session instructions &optional opts)
  "Run INSTRUCTIONS synchronously in SESSION context.
Returns a structured result plist."
  (let ((env (flywire-session-env session)))
    (funcall (flywire-env-run env)
             (lambda ()
               (let ((res (flywire--execute-steps-with-result session instructions opts)))
                 (when (plist-get opts :snapshot-after)
                   (setq res (plist-put res :snapshot
                                        (funcall (flywire-env-snapshot env)))))
                 res)))))

(defun flywire-session-start-async (session instructions &optional opts)
  "Run INSTRUCTIONS asynchronously in SESSION context.
Returns \\='started immediately.  OPTS are currently ignored."
  (ignore opts)
  (let ((env (flywire-session-env session)))
    (funcall (flywire-env-run env)
             (lambda ()
               (run-with-timer 0 nil
                               (lambda ()
                                 (flywire--emit-event session `(:type :exec-start :session ,session))
                                 (dolist (step instructions)
                                   (flywire--emit-event session `(:type :step-start :session ,session :action ,step))
                                   (condition-case err
                                       (progn
                                         (flywire--execute-step step)
                                         (flywire--emit-event session `(:type :step-end :session ,session :action ,step :status :ok)))
                                     (error
                                      (flywire--emit-event session `(:type :step-end :session ,session :action ,step :status :error :error ,err)))))
                                 (flywire--emit-event session `(:type :exec-complete :session ,session)))))))
  'started)

;;; Shims (Backward Compatibility)

;;;###autoload
(defun flywire-do (instructions)
  "Execute INSTRUCTIONS and return a fresh snapshot plist.
INSTRUCTIONS must be a list of action alists (e.g., parsed from JSON).
The function ensures inputs are queued before commands run, then returns
`flywire-snapshot-get-snapshot` for the updated state.
Signals an error if execution fails."
  (let* ((session (flywire-session-current))
         (res (flywire-session-exec session instructions '(:snapshot-after t))))
    (when (eq (plist-get res :status) :error)
      (let ((err-info (plist-get res :error)))
        (signal (car (plist-get err-info :data))
                (cdr (plist-get err-info :data)))))
    (plist-get res :snapshot)))

;;;###autoload
(defun flywire-do-async (instructions)
  "Execute INSTRUCTIONS asynchronously.
INSTRUCTIONS must be a list of action alists.
Returns \='started symbol immediately.  The actions are scheduled on a timer.
State updates will be pushed via `flywire-async-output-handler`."
  (flywire-session-start-async (flywire-session-current) instructions)
  'started)

(provide 'flywire)
;;; flywire.el ends here
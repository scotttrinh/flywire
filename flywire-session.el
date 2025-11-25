;;; flywire-session.el --- Session and environment management for flywire -*- lexical-binding: t; -*-

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

;; Defines the `flywire-session` and `flywire-session-env` abstractions.

;;; Code:

(require 'cl-lib)
(require 'flywire-action)
(require 'flywire-snapshot)
(require 'flywire-policy)

;;; Structures

(cl-defstruct flywire-session-env
  name
  (run #'funcall)           ; (lambda (thunk)) -> execute within this env
  (snapshot #'flywire-snapshot-get-snapshot) ; (lambda (&optional profile)) -> snapshot plist
  (enable-events #'ignore) ; (lambda (session opts)) -> install hooks/timers, optional
  (teardown #'ignore))     ; (lambda (session)) -> optional cleanup

(cl-defstruct (flywire-session
               (:constructor flywire-session--create))
  id
  env          ; flywire-session-env instance
  options      ; plist of configuration options
  state        ; internal state (messages, throttling, etc.)
  handlers)    ; event handlers / callbacks

(defvar flywire-session--current nil
  "Global default session.")

(defun flywire-session--resolve-safety-policy (session)
  "Return the safety policy predicate for SESSION.
Falls back to the global `flywire-policy-allow-command-p`."
  (or (plist-get (flywire-session-options session) :safety-policy)
      flywire-policy-allow-command-p))

;;; Core Session API

(defun flywire-session--default-enable-events (_session _opts)
  "Default event setup: do nothing for now."
  ;; This can be expanded to replicate flywire-async behavior scoped to the session
  nil)

(defun flywire-session-env-default ()
  "Return a default environment representing the current Emacs session."
  (make-flywire-session-env
   :name "default"
   :run #'funcall
   :snapshot #'flywire-snapshot-get-snapshot
   :enable-events #'flywire-session--default-enable-events))

(defun flywire-session-create (&rest args)
  "Create a new session.
ARGS can include :id, :env, :snapshot-profile, :safety-policy, :events."
  (let ((env (plist-get args :env))
        (id (or (plist-get args :id) (gensym "flywire-session-"))))
    (flywire-session--create
     :id id
     :env (or env (flywire-session-env-default))
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
  "Ask the SESSION environment to set up any required hooks/timers.
OPTS are passed to the environment's enable-events function."
  (let ((env (flywire-session-env session)))
    (funcall (flywire-session-env-enable-events env) session opts)))

(defun flywire-session--emit-event (session event)
  "Emit EVENT to handlers on SESSION."
  (let ((handlers (flywire-session-handlers session)))
    (dolist (h handlers)
      (funcall h event))))

;;; Execution Logic

(defun flywire-session--with-step-context (buffer-name window-id thunk)
  "Execute THUNK in the context of BUFFER-NAME and/or WINDOW-ID if present."
  (let ((target-window (and window-id (flywire-snapshot-find-window window-id)))
        (target-buffer (and buffer-name (get-buffer buffer-name))))
    (cond
     ((and target-window target-buffer)
      (with-selected-window target-window
        (with-current-buffer target-buffer
          (funcall thunk))))
     (target-window
      (with-selected-window target-window
        (funcall thunk)))
     (target-buffer
      (with-current-buffer target-buffer
        (funcall thunk)))
     (t (funcall thunk)))))

(defun flywire-session--execute-step (step)
  "Execute STEP, which comes from a parsed JSON instruction.
STEP should be an alist with at least an `action` entry."
  (let* ((action-name (or (alist-get "action" step nil nil #'string=)
                          (alist-get 'action step)))
         (buffer-name (or (alist-get "buffer" step nil nil #'string=)
                          (alist-get 'buffer step)))
         (window-id (or (alist-get "window-id" step nil nil #'string=)
                        (alist-get 'window-id step)))
         (tool-name (pcase action-name
                      ((or "type" 'type) "type_text")
                      ((or "key" 'key) "press_key")
                      ((or "command" 'command) "run_command")
                      (_ action-name))))
    (if tool-name
        (flywire-session--with-step-context buffer-name window-id
          (lambda ()
            (flywire-action-execute-tool tool-name step)))
      (error "Flywire: unknown action %S" step))))

(defun flywire-session--capture-messages (thunk)
  "Run THUNK and return any new text added to *Messages*."
  (let* ((msg-buf (get-buffer-create "*Messages*"))
         (start (with-current-buffer msg-buf (point-max))))
    (funcall thunk)
    (with-current-buffer msg-buf
      (if (> (point-max) start)
          (buffer-substring-no-properties start (point-max))
        nil))))

(defun flywire-session--execute-steps-with-result (session instructions &optional opts)
  "Execute INSTRUCTIONS using SESSION and OPTS.
Returns a result plist.  Captures errors and messages.
Honors SESSION safety policy."
  (ignore opts)
  (let ((step-results nil)
        (status :ok)
        (error-info nil)
        (captured-messages nil)
        (policy (flywire-session--resolve-safety-policy session)))
    (let ((flywire-policy-allow-command-p policy))
      (setq captured-messages
            (flywire-session--capture-messages
             (lambda ()
               (condition-case err
                   (dolist (step instructions)
                     (flywire-session--execute-step step)
                     (push `((:step-index . ,(length step-results))
                             (:action . ,step)
                             (:status . :ok))
                           step-results))
                 (quit
                  (setq status :cancelled)
                  (setq error-info '(:type quit :message "Cancelled")))
                 (error
                  (setq status :error)
                  (setq error-info `(:type error :message ,(error-message-string err) :data ,err))))))))
    (list :status status
          :steps (nreverse step-results)
          :messages captured-messages
          :error error-info)))

(defun flywire-session-exec (session instructions &optional opts)
  "Run INSTRUCTIONS synchronously in SESSION context.
Returns a structured result plist.
OPTS can contain :snapshot-after (boolean or profile name)."
  (let ((env (flywire-session-env session))
        (session-profile (plist-get (flywire-session-options session) :snapshot-profile)))
    (funcall (flywire-session-env-run env)
             (lambda ()
               (let ((res (flywire-session--execute-steps-with-result session instructions opts)))
                 (let ((snap-opt (plist-get opts :snapshot-after)))
                   (when snap-opt
                     (let ((profile (if (and (symbolp snap-opt) (not (eq snap-opt t)))
                                        snap-opt
                                      session-profile)))
                       (setq res (plist-put res :snapshot
                                            (funcall (flywire-session-env-snapshot env) profile))))))
                 res)))))

(defun flywire-session-start-async (session instructions &optional opts)
  "Run INSTRUCTIONS asynchronously in SESSION context.
Returns \='started immediately.
If :snapshot-profile is set in SESSION options, snapshots are emitted with
:step-end events."
  (ignore opts)
  (let ((env (flywire-session-env session))
        (session-profile (plist-get (flywire-session-options session) :snapshot-profile))
        (policy (flywire-session--resolve-safety-policy session)))
    (funcall (flywire-session-env-run env)
             (lambda ()
               (run-with-timer 0 nil
                               (lambda ()
                                 (let ((flywire-policy-allow-command-p policy))
                                   (flywire-session--emit-event session `(:type :exec-start :session ,session))
                                   (dolist (step instructions)
                                     (flywire-session--emit-event session `(:type :step-start :session ,session :action ,step))
                                     (condition-case err
                                         (let ((step-res nil)) (flywire-session--execute-step step)
                                           (setq step-res `(:type :step-end :session ,session :action ,step :status :ok))
                                           ;; Attach snapshot if configured
                                           (when session-profile
                                             (setq step-res (plist-put step-res :snapshot
                                                                       (funcall (flywire-session-env-snapshot env) session-profile))))
                                           (flywire-session--emit-event session step-res))
                                       (error
                                        (flywire-session--emit-event session `(:type :step-end :session ,session :action ,step :status :error :error ,err)))))
                                   (flywire-session--emit-event session `(:type :exec-complete :session ,session))))))))
  'started)

(provide 'flywire-session)
;;; flywire-session.el ends here

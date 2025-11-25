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
(require 'subr-x)
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

(cl-defstruct flywire-session-async-handle
  "Handle for in-flight asynchronous execution."
  id
  session
  timer
  status
  result
  cancel-reason
  opts)

(defvar flywire-session--async-counter 0
  "Counter for generating async handle ids.")

(defvar flywire-session--current nil
  "Global default session.")

(defun flywire-session--resolve-safety-policy (session opts)
  "Return the safety policy predicate for SESSION using OPTS overrides.
Falls back to the global `flywire-policy-allow-command-p`."
  (or (plist-get opts :safety-policy)
      (plist-get (flywire-session-options session) :safety-policy)
      flywire-policy-allow-command-p))

;;; Core Session API

(defvar flywire-session--default-event-state (make-hash-table :test 'eq)
  "Tracking table for default env event hooks keyed by session.")

(defun flywire-session--default-teardown-events (session)
  "Remove default event wiring for SESSION."
  (let ((state (gethash session flywire-session--default-event-state)))
    (when state
      (when-let ((hook (plist-get state :minibuffer-hook)))
        (remove-hook 'minibuffer-setup-hook hook))
      (when-let ((timer (plist-get state :idle-timer)))
        (cancel-timer timer))
      (remhash session flywire-session--default-event-state))))

(defun flywire-session--default-enable-events (session opts)
  "Install default minibuffer/idle event emitters for SESSION using OPTS."
  (unless (gethash session flywire-session--default-event-state)
    (let* ((env (flywire-session-env session))
           (run-fn (flywire-session-env-run env))
           (snapshot-fn (flywire-session-env-snapshot env))
           (snapshot-profile (or (flywire-session--snapshot-profile session t
                                                                     (plist-get (flywire-session-options session) :snapshot-profile))
                                 (plist-get (flywire-session-options session) :snapshot-profile)))
           (idle-delay (or (plist-get opts :idle-delay) 0.5))
           (minibuffer-hook
            (lambda ()
              (funcall run-fn
                       (lambda ()
                         (flywire-session--emit-event-maybe
                          session opts
                          (list :type :minibuffer-open
                                :session session
                                :snapshot (funcall snapshot-fn snapshot-profile)))))))
           (idle-handler
            (lambda ()
              (funcall run-fn
                       (lambda ()
                         (flywire-session--emit-event-maybe
                          session opts
                          (list :type :idle
                                :session session
                                :snapshot (funcall snapshot-fn snapshot-profile)))))))
           (idle-timer (run-with-idle-timer idle-delay t idle-handler)))
      (add-hook 'minibuffer-setup-hook minibuffer-hook)
      (puthash session
               (list :minibuffer-hook minibuffer-hook
                     :idle-timer idle-timer
                     :opts opts)
               flywire-session--default-event-state))))

(defun flywire-session-env-default ()
  "Return a default environment representing the current Emacs session."
  (make-flywire-session-env
   :name "default"
   :run #'funcall
   :snapshot #'flywire-snapshot-get-snapshot
   :enable-events #'flywire-session--default-enable-events
   :teardown #'flywire-session--default-teardown-events))

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

(defun flywire-session--register-async-handle (session handle)
  "Track HANDLE as the active async execution for SESSION."
  (setf (flywire-session-state session)
        (plist-put (flywire-session-state session) :async-handle handle))
  handle)

(defun flywire-session--active-async-handle (session)
  "Return the active async handle for SESSION, if any."
  (plist-get (flywire-session-state session) :async-handle))

(defun flywire-session--event-enabled-p (session opts type)
  "Return non-nil if event TYPE should be emitted for SESSION given OPTS."
  (let ((events (or (plist-get opts :events)
                    (plist-get (flywire-session-options session) :events)
                    :all)))
    (cond
     ((or (eq events :all) (null events)) t)
     ((eq events :none) nil)
     ((listp events) (memq type events))
     (t t))))

(defun flywire-session-on-event (session handler)
  "Register HANDLER for events on SESSION."
  (push handler (flywire-session-handlers session))
  handler)

(defun flywire-session-off-event (session handler)
  "Unregister HANDLER from SESSION events."
  (setf (flywire-session-handlers session)
        (delq handler (flywire-session-handlers session))))

(defun flywire-session-enable-events (session &optional opts)
  "Ask the SESSION environment to set up any required hooks/timers.
OPTS are passed to the environment's enable-events function."
  (let ((env (flywire-session-env session)))
    (funcall (flywire-session-env-run env)
             (lambda ()
               (funcall (flywire-session-env-enable-events env) session opts)))))

(defun flywire-session-disable-events (session)
  "Tear down any event hooks/timers for SESSION via the environment."
  (let ((env (flywire-session-env session)))
    (funcall (flywire-session-env-run env)
             (lambda ()
               (funcall (flywire-session-env-teardown env) session)))))

(defun flywire-session--emit-event (session event)
  "Emit EVENT to handlers on SESSION."
  (let ((handlers (flywire-session-handlers session)))
    (dolist (h handlers)
      (funcall h event))))

(defun flywire-session--emit-event-maybe (session opts event)
  "Emit EVENT for SESSION if enabled by OPTS or session defaults."
  (when (flywire-session--event-enabled-p session opts (plist-get event :type))
    (flywire-session--emit-event session event)))

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

(defun flywire-session--capture-message-delta (buffer start-pos)
  "Return list of messages appended to BUFFER since START-POS."
  (with-current-buffer buffer
    (when (> (point-max) start-pos)
      (split-string (buffer-substring-no-properties start-pos (point-max)) "\n" t))))

(defun flywire-session--snapshot-profile (session value &optional fallback)
  "Resolve snapshot profile for SESSION from VALUE with optional FALLBACK.
If VALUE is t, prefer FALLBACK or the session's :snapshot-profile.  If VALUE
is a symbol, use it directly.  A nil return value means use default collectors."
  (let ((session-profile (or fallback (plist-get (flywire-session-options session) :snapshot-profile))))
    (cond
     ((eq value t) session-profile)
     ((symbolp value) value)
     (t nil))))

(defun flywire-session--execute-steps-with-result (session instructions &optional opts)
  "Execute INSTRUCTIONS using SESSION and OPTS.
Returns a result plist that includes per-step messages/errors/snapshots.
Honors safety policy overrides from OPTS."
  (let* ((env (flywire-session-env session))
         (snapshot-fn (flywire-session-env-snapshot env))
         (session-snapshot-profile (or (plist-get opts :snapshot-profile)
                                       (plist-get (flywire-session-options session) :snapshot-profile)))
         (policy (flywire-session--resolve-safety-policy session opts))
         (cancelled-p (or (plist-get opts :cancelled-p) (lambda () nil)))
         (on-step-start (plist-get opts :on-step-start))
         (on-step-end (plist-get opts :on-step-end))
         (snap-before-opt (plist-get opts :snapshot-before))
         (snap-after-opt (plist-get opts :snapshot-after))
         (step-snapshot-profile
          (let ((opt (plist-get opts :snapshot-per-step)))
            (cond
             ;; Explicit opt-out
             ((and (plist-member opts :snapshot-per-step) (not opt)) nil)
             ((plist-member opts :snapshot-per-step)
              (flywire-session--snapshot-profile session opt session-snapshot-profile))
             (t session-snapshot-profile))))
         (message-buffer (get-buffer-create "*Messages*"))
         (messages-start (with-current-buffer message-buffer (point-max)))
         (status :ok)
         (error-info nil)
         (step-results nil)
         (step-index 0)
         (snapshot-before nil)
         (snapshot-after nil))
    (when snap-before-opt
      (setq snapshot-before (funcall snapshot-fn (flywire-session--snapshot-profile session snap-before-opt session-snapshot-profile))))
    (let ((flywire-policy-allow-command-p policy))
      (cl-block :steps
        (dolist (step instructions)
          (let ((cancelled (funcall cancelled-p)))
            (when cancelled
              (setq status :cancelled)
              (setq error-info
                    (or error-info
                        (if (stringp cancelled)
                            `(:type cancel :message ,cancelled)
                          '(:type cancel :message "Cancelled"))))
              (cl-return-from :steps)))
          (when on-step-start (funcall on-step-start step-index step))
          (let* ((step-start (with-current-buffer message-buffer (point-max)))
                 (step-status :ok)
                 (step-error nil)
                 (step-messages nil)
                 (step-snapshot nil))
            (condition-case err
                (flywire-session--execute-step step)
              (quit
               (setq step-status :cancelled)
               (setq status :cancelled)
               (setq step-error '(:type quit :message "Cancelled"))
               (setq error-info step-error))
              (error
               (setq step-status :error)
               (setq status :error)
               (setq step-error `(:type error :message ,(error-message-string err) :data ,err))
               (setq error-info step-error)))
            (setq step-messages (flywire-session--capture-message-delta message-buffer step-start))
            (when step-snapshot-profile
              (setq step-snapshot (funcall snapshot-fn step-snapshot-profile)))
            (let ((step-res (list :step-index step-index
                                  :action step
                                  :status step-status)))
              (when step-messages (setq step-res (plist-put step-res :messages step-messages)))
              (when step-error (setq step-res (plist-put step-res :error step-error)))
              (when step-snapshot (setq step-res (plist-put step-res :snapshot step-snapshot)))
              (push step-res step-results)
              (when on-step-end (funcall on-step-end step-res)))
            (cl-incf step-index)
            (when (memq step-status '(:cancelled :error))
              (cl-return-from :steps)))))) ; stop on first cancellation/error
    (when snap-after-opt
      (setq snapshot-after (funcall snapshot-fn (flywire-session--snapshot-profile session snap-after-opt session-snapshot-profile))))
    (let ((captured-messages (flywire-session--capture-message-delta message-buffer messages-start))
          (result (list :status status
                        :steps (nreverse step-results))))
      (when captured-messages (setq result (plist-put result :messages captured-messages)))
      (when error-info (setq result (plist-put result :error error-info)))
      (when snapshot-before (setq result (plist-put result :snapshot-before snapshot-before)))
      (when snapshot-after
        (setq result (plist-put result :snapshot-after snapshot-after))
        ;; Maintain legacy :snapshot key for callers expecting it
        (setq result (plist-put result :snapshot snapshot-after)))
      result)))

(defun flywire-session-exec (session instructions &optional opts)
  "Run INSTRUCTIONS synchronously in SESSION context.
Returns a structured result plist.
OPTS can contain :snapshot-after (boolean or profile name)."
  (let ((env (flywire-session-env session)))
    (funcall (flywire-session-env-run env)
             (lambda ()
               (flywire-session--execute-steps-with-result session instructions opts)))))

(defun flywire-session-start-async (session instructions &optional opts)
  "Run INSTRUCTIONS asynchronously in SESSION context.
Returns a handle object that can be used with `flywire-session-cancel`.
If :snapshot-profile is set in SESSION options, snapshots are attached to
:step-end events and results."
  (let* ((env (flywire-session-env session))
         (run-fn (flywire-session-env-run env))
         (handle (make-flywire-session-async-handle
                  :id (format "flywire-async-%d" (cl-incf flywire-session--async-counter))
                  :session session
                  :status :scheduled
                  :opts opts)))
    (flywire-session--register-async-handle session handle)
    (setf (flywire-session-async-handle-timer handle)
          (run-with-timer
           0 nil
           (lambda ()
             (funcall run-fn
                      (lambda ()
                        (let ((cancel-reason (flywire-session-async-handle-cancel-reason handle)))
                          (if cancel-reason
                              (let ((result `(:status :cancelled :error (:type cancel :message ,cancel-reason))))
                                (setf (flywire-session-async-handle-status handle) :cancelled)
                                (setf (flywire-session-async-handle-result handle) result)
                                (flywire-session--emit-event-maybe
                                 session opts
                                 `(:type :exec-complete
                                         :session ,session
                                         :handle ,(flywire-session-async-handle-id handle)
                                         :result ,result))
                                (setf (flywire-session-async-handle-timer handle) nil))
                            (setf (flywire-session-async-handle-status handle) :running)
                            (flywire-session--emit-event-maybe
                             session opts
                             `(:type :exec-start
                                     :session ,session
                                     :handle ,(flywire-session-async-handle-id handle)))
                            (let* ((exec-opts (plist-put opts :cancelled-p
                                                         (lambda ()
                                                           (flywire-session-async-handle-cancel-reason handle))))
                                   (exec-opts (plist-put exec-opts :on-step-start
                                                         (lambda (idx step)
                                                           (flywire-session--emit-event-maybe
                                                            session opts
                                                            `(:type :step-start
                                                                    :session ,session
                                                                    :handle ,(flywire-session-async-handle-id handle)
                                                                    :step-index ,idx
                                                                    :action ,step)))))
                                   (exec-opts (plist-put exec-opts :on-step-end
                                                         (lambda (step-res)
                                                           (let ((event (copy-sequence step-res)))
                                                             (setq event (plist-put event :type :step-end))
                                                             (setq event (plist-put event :session session))
                                                             (setq event (plist-put event :handle (flywire-session-async-handle-id handle)))
                                                             (flywire-session--emit-event-maybe session opts event)))))
                                   (result
                                    (flywire-session--execute-steps-with-result
                                     session instructions exec-opts)))
                              (setf (flywire-session-async-handle-result handle) result)
                              (setf (flywire-session-async-handle-status handle) (plist-get result :status))
                              (flywire-session--emit-event-maybe
                               session opts
                               `(:type :exec-complete
                                       :session ,session
                                       :handle ,(flywire-session-async-handle-id handle)
                                       :result ,result))
                              (setf (flywire-session-async-handle-timer handle) nil)))))))))
    handle))

(defun flywire-session-cancel (session-or-handle &optional cause)
  "Request cancellation of in-flight async execution.
SESSION-OR-HANDLE can be a session or a specific async handle.
CAUSE is an optional string describing the reason."
  (let* ((handle (if (flywire-session-async-handle-p session-or-handle)
                     session-or-handle
                   (flywire-session--active-async-handle session-or-handle)))
         (session (if (flywire-session-async-handle-p session-or-handle)
                      (flywire-session-async-handle-session session-or-handle)
                    session-or-handle)))
    (unless handle
      (error "Flywire: no async execution to cancel"))
    (let* ((reason (or cause (flywire-session-async-handle-cancel-reason handle) "Cancelled"))
           (result `(:status :cancelled :error (:type cancel :message ,reason)))
           (opts (flywire-session-async-handle-opts handle)))
      (setf (flywire-session-async-handle-cancel-reason handle) reason)
      (setf (flywire-session-async-handle-status handle) :cancelled)
      (setf (flywire-session-async-handle-result handle) result)
      (let ((timer (flywire-session-async-handle-timer handle)))
        (when (timerp timer)
          (cancel-timer timer)
          (setf (flywire-session-async-handle-timer handle) nil)))
      (flywire-session--emit-event-maybe
       session opts
       `(:type :exec-cancelled
               :session ,session
               :handle ,(flywire-session-async-handle-id handle)
               :reason ,reason
               :result ,result))
      (flywire-session--emit-event-maybe
       session opts
       `(:type :exec-complete
               :session ,session
               :handle ,(flywire-session-async-handle-id handle)
               :result ,result))
      :cancelled)))

(provide 'flywire-session)
;;; flywire-session.el ends here

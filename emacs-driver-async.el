;;; emacs-driver-async.el --- Async execution and monitoring for emacs-driver -*- lexical-binding: t; -*-

;;; Commentary:
;; Handles reactive execution: triggering commands and waiting for UI updates
;; (idle timers, minibuffer hooks) to report state back to the agent.

;;; Code:

(require 'emacs-driver-snapshot)
(require 'json)

(defvar emacs-driver-output-handler #'emacs-driver-default-output-handler
  "Function to call with the snapshot plist when state changes.")

(defun emacs-driver-default-output-handler (snapshot)
  "Default handler: print SNAPSHOT as JSON to stdout."
  (message "EMACS_DRIVER_SNAPSHOT:%s" (json-encode snapshot)))

(defvar emacs-driver--idle-timer nil)

(define-minor-mode emacs-driver-async-mode
  "Toggle asynchronous monitoring of Emacs state."
  :global t
  (if emacs-driver-async-mode
      (emacs-driver--enable-monitoring)
    (emacs-driver--disable-monitoring)))

(defun emacs-driver--enable-monitoring ()
  "Setup hooks and timers."
  (add-hook 'minibuffer-setup-hook #'emacs-driver--on-minibuffer-setup)
  ;; Cancel existing timer if any
  (when emacs-driver--idle-timer (cancel-timer emacs-driver--idle-timer))
  ;; Set new idle timer (e.g., 0.5s)
  (setq emacs-driver--idle-timer
        (run-with-idle-timer 0.5 t #'emacs-driver--on-idle)))

(defun emacs-driver--disable-monitoring ()
  "Teardown hooks and timers."
  (remove-hook 'minibuffer-setup-hook #'emacs-driver--on-minibuffer-setup)
  (when emacs-driver--idle-timer
    (cancel-timer emacs-driver--idle-timer)
    (setq emacs-driver--idle-timer nil)))

(defun emacs-driver--on-minibuffer-setup ()
  "Called when minibuffer opens."
  ;; Slight delay to ensure minibuffer content is ready?
  ;; Usually setup hook is run when buffer is ready.
  (funcall emacs-driver-output-handler (emacs-driver-get-snapshot)))

(defun emacs-driver--on-idle ()
  "Called when Emacs is idle."
  (funcall emacs-driver-output-handler (emacs-driver-get-snapshot)))

(provide 'emacs-driver-async)
;;; emacs-driver-async.el ends here

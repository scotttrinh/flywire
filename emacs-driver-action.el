;;; emacs-driver-action.el --- Action executors for emacs-driver -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers that queue user-like input and run interactive commands for the
;; higher-level emacs-driver orchestrator.

;;; Code:

(require 'cl-lib)

(defun emacs-driver-simulate-keys (key-sequence)
  "Push KEY-SEQUENCE (\"C-x C-f\", \"RET\", etc.) into `unread-command-events'."
  (let ((events (condition-case err
                    (listify-key-sequence (kbd key-sequence))
                  (error
                   (message "emacs-driver: failed to parse key sequence %s: %s" key-sequence err)
                   nil))))
    (when events
      (setq unread-command-events
            (append unread-command-events events)))))

(defun emacs-driver-push-input (input-string)
  "Queue each character of INPUT-STRING into `unread-command-events'."
  (when (and input-string (stringp input-string))
    (setq unread-command-events
          (append unread-command-events
                  (string-to-list input-string)))))

(defun emacs-driver-execute-tool (tool-name args)
  "Run TOOL-NAME with ARGS in the pre-load style described in the roadmap.
ARGs should be an alist like ((name . "find-file"))."
  (pcase tool-name
    ("type_text"
     (emacs-driver-push-input (alist-get 'text args)))
    ("press_key"
     (emacs-driver-simulate-keys (alist-get 'key args)))
    ("run_command"
     (let ((command-name (alist-get 'name args)))
       (when (and (stringp command-name)
                  (commandp (intern command-name)))
         (call-interactively (intern command-name)))))
    (_
     (message "emacs-driver: unknown tool %s" tool-name))))

(provide 'emacs-driver-action)
;;; emacs-driver-action.el ends here

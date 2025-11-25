;;; flywire-action.el --- Action executors for flywire -*- lexical-binding: t; -*-

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

;; Helpers that queue user-like input and run interactive commands for the
;; higher-level flywire orchestrator.

;;; Code:

(require 'cl-lib)
(require 'flywire-policy)

(defun flywire-action-simulate-keys (key-sequence)
  "Push KEY-SEQUENCE (e.g., \"\\<global-map>\\[find-file]\", \"RET\") into `unread-command-events'."
  (let ((inhibit-quit t))
    (let ((events (condition-case err
                      (listify-key-sequence (kbd key-sequence))
                    (error
                     (message "flywire: failed to parse key sequence %s: %s" key-sequence err)
                     nil))))
      (when events
        (setq unread-command-events
              (append unread-command-events events))))))

(defun flywire-action-push-input (input-string)
  "Queue each character of INPUT-STRING into `unread-command-events'."
  (let ((inhibit-quit t))
    (when (and input-string (stringp input-string))
      (setq unread-command-events
            (append unread-command-events
                    (string-to-list input-string))))))

(defvar flywire-action-registry (make-hash-table :test 'equal)
  "Registry of available action tools.")

(defun flywire-action-register (name fn &optional plist)
  "Register tool NAME with function FN and metadata PLIST."
  (puthash name (list :fn fn :meta plist) flywire-action-registry))

(defun flywire-action-type-text (args)
  "Implementation of type_text tool.
ARGS is an alist containing the text to type."
  (flywire-action-push-input (alist-get "text" args nil nil #'string=)))

(defun flywire-action-press-key (args)
  "Implementation of press_key tool.
ARGS is an alist containing the key or chord to press."
  (let ((key (or (alist-get "key" args nil nil #'string=)
                 (alist-get "chord" args nil nil #'string=))))
    (flywire-action-simulate-keys key)))

(defun flywire-action-run-command (args)
  "Implementation of run_command tool.
ARGS is an alist containing the command name."
  (let* ((command-name (alist-get "name" args nil nil #'string=))
         (sym (and (stringp command-name) (intern-soft command-name))))
    (cond
     ((not (and sym (commandp sym)))
      (error "Flywire: invalid command %s" command-name))
     ((not (funcall flywire-policy-allow-command-p sym))
      (error "Flywire: command denied by policy: %s" command-name))
     (t
      (call-interactively sym)))))

(defun flywire-action-cancel (_args)
  "Implementation of cancel tool (simulates `keyboard-quit')."
  (signal 'quit nil))

(defun flywire-action-goto-location (args)
  "Implementation of goto_location tool.
ARGS can contain `point` (int), `line` (int), or `column` (int)."
  (let ((p (alist-get "point" args nil nil #'string=))
        (l (alist-get "line" args nil nil #'string=))
        (c (alist-get "column" args nil nil #'string=)))
    (when (integerp p)
      (goto-char p))
    (when (integerp l)
      (goto-char (point-min))
      (forward-line (1- l)))
    (when (integerp c)
      (move-to-column c))))

;; Register default tools
(flywire-action-register "type_text" #'flywire-action-type-text)
(flywire-action-register "press_key" #'flywire-action-press-key)
(flywire-action-register "run_command" #'flywire-action-run-command)
(flywire-action-register "cancel" #'flywire-action-cancel)
(flywire-action-register "goto_location" #'flywire-action-goto-location)

(defun flywire-action-execute-tool (tool-name args)
  "Execute TOOL-NAME with ARGS using the registry.
ARGS is an alist (typically with string keys from JSON)."
  (let ((entry (gethash tool-name flywire-action-registry)))
    (if entry
        (funcall (plist-get entry :fn) args)
      (error "Flywire: unknown tool %s" tool-name))))

(provide 'flywire-action)
;;; flywire-action.el ends here

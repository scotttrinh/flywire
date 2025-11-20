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

(defun flywire-action-execute-tool (tool-name args)
  "Run TOOL-NAME with ARGS in the pre-load style described in the roadmap.
ARGS should be an alist like ((name . \"find-file\"))."
  (pcase tool-name
    ("type_text"
     (flywire-action-push-input (alist-get 'text args)))
    ("press_key"
     (flywire-action-simulate-keys (alist-get 'key args)))
    ("run_command"
     (let* ((command-name (alist-get 'name args))
            (sym (and (stringp command-name) (intern-soft command-name))))
       (when (and sym (commandp sym))
         (call-interactively sym))))
    (_
     (error "Flywire: unknown tool %s" tool-name))))

(provide 'flywire-action)
;;; flywire-action.el ends here

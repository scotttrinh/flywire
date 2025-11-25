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
(require 'flywire-policy)
(require 'flywire-snapshot)
(require 'flywire-action)
(require 'flywire-session)
(require 'flywire-async)

;; Global aliases to match public API naming and legacy references.
(defvaralias 'flywire-allow-command-p 'flywire-policy-allow-command-p)
(defvaralias 'flywire-action-allow-command-p 'flywire-policy-allow-command-p)

(defgroup flywire nil
  "Arms and eyes for automated agents."
  :group 'tools
  :prefix "flywire-")

(defun flywire--allow-all (_command)
  "Default safety policy: allow all commands.
_COMMAND is the command symbol being checked."
  t)

(defcustom flywire-allow-command-p #'flywire--allow-all
  "Predicate to decide whether a command may be run.
Called with one argument, the command symbol."
  :group 'flywire
  :type 'function)

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

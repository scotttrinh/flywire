;;; emacs-driver.el --- "Arms and eyes" for automated agents -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Scott Trinh

;; Author: Scott Trinh <scott@scotttrinh.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: processes, tools, extensions
;; URL: https://github.com/scotttrinh/emacs-driver

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

;; Public surface for the emacs-driver package. This module coordinates
;; snapshots and tool execution so an external agent can see Emacs state
;; and drive it via scripted steps.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'emacs-driver-snapshot)
(require 'emacs-driver-action)
(require 'emacs-driver-async)

(defun emacs-driver--execute-step (step)
  "Execute STEP, which comes from a parsed JSON instruction.
STEP should be an alist with at least an `action` entry." 
  (pcase (alist-get 'action step nil nil #'string=)
    ("type"
     (emacs-driver-push-input (alist-get 'text step)))
    ("key"
     (emacs-driver-simulate-keys (alist-get 'chord step)))
    ("command"
     (emacs-driver-execute-tool "run_command"
                                `((name . ,(alist-get 'name step)))))
    (_
     (message "Unknown emacs-driver action: %S" step))))

;;;###autoload
(defun emacs-driver-do (instruction-json)
  "Execute INSTRUCTION-JSON and return a fresh snapshot plist.
Instructions are JSON arrays of action objects. The function ensures
inputs are queued before commands run, then returns
`emacs-driver-get-snapshot` for the updated state." 
  (let ((instructions (json-parse-string instruction-json :object-type 'alist :array-type 'list)))
    (dolist (step instructions)
      (emacs-driver--execute-step step))
    (emacs-driver-get-snapshot)))

;;;###autoload
(defun emacs-driver-do-async (instruction-json)
  "Execute INSTRUCTION-JSON asynchronously.
Returns 'started symbol immediately. The actions are scheduled on a timer.
State updates will be pushed via `emacs-driver-output-handler`."
  (let ((instructions (json-parse-string instruction-json :object-type 'alist :array-type 'list)))
    (run-with-timer 0 nil
                    (lambda ()
                      (dolist (step instructions)
                        (emacs-driver--execute-step step)))))
  'started)

(provide 'emacs-driver)
;;; emacs-driver.el ends here

;;; flywire-policy.el --- Safety and policy hooks for flywire -*- lexical-binding: t; -*-

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

;; Defines global safety/policy hooks used by flywire to gate command
;; execution.  Session or environment code can override these defaults.

;;; Code:

(defun flywire-policy-allow-all (_command)
  "Default safety policy: allow all commands.
_COMMAND is the command symbol being checked."
  t)

(defcustom flywire-policy-allow-command-p #'flywire-policy-allow-all
  "Predicate to decide whether a command may be run.
Called with one argument, the command symbol."
  :group 'flywire
  :type 'function)

(provide 'flywire-policy)
;;; flywire-policy.el ends here

;;; flywire-async.el --- Async execution and monitoring for flywire -*- lexical-binding: t; -*-

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

;; Handles reactive execution: triggering commands and waiting for UI updates
;; (idle timers, minibuffer hooks) to report state back to the agent.

;;; Code:

(require 'flywire-snapshot)
(require 'flywire-session)
(require 'flywire-policy)
(require 'json)

(declare-function flywire-session--snapshot-profile "flywire-session" (session value &optional fallback))

(defcustom flywire-async-output-handler #'flywire-async-default-output-handler
  "Function to call with the snapshot plist when state changes.
The function receives a single argument: the snapshot plist."
  :type 'function
  :group 'flywire)

(defun flywire-async-default-output-handler (snapshot)
  "Default handler: print SNAPSHOT as JSON to stdout."
  (message "EMACS_DRIVER_SNAPSHOT:%s" (json-encode snapshot)))

(defvar flywire-async--session-handlers (make-hash-table :test 'eq)
  "Map of sessions to their async event handlers.")

(defun flywire-async--snapshot-for-session (session &optional profile)
  "Collect a snapshot for SESSION using PROFILE if provided."
  (let* ((env (flywire-session-env session))
         (run-fn (flywire-session-env-run env))
         (snap-fn (flywire-session-env-snapshot env))
         (profile (or profile (plist-get (flywire-session-options session) :snapshot-profile))))
    (funcall run-fn
             (lambda ()
               (funcall snap-fn (flywire-session--snapshot-profile session t profile))))))

(defun flywire-async--event->snapshot (event)
  "Extract or build a snapshot for EVENT."
  (or (plist-get event :snapshot)
      (when-let ((result (plist-get event :result)))
        (or (plist-get result :snapshot-after)
            (plist-get result :snapshot)
            (plist-get result :snapshot-before)))
      (let ((session (plist-get event :session)))
        (when (flywire-session-p session)
          (flywire-async--snapshot-for-session session)))))

(defun flywire-async--session-adapter (event)
  "Forward session EVENT to the output handler."
  (let ((type (plist-get event :type)))
    (when (memq type '(:step-end :exec-complete :exec-cancelled :minibuffer-open :idle))
      (when-let ((snap (flywire-async--event->snapshot event)))
        (funcall flywire-async-output-handler snap)))))

(defun flywire-async--attach-session (session)
  "Register handlers and enable events for SESSION."
  (unless (gethash session flywire-async--session-handlers)
    (let ((handler (lambda (event) (flywire-async--session-adapter event))))
      (puthash session handler flywire-async--session-handlers)
      (flywire-session-on-event session handler)
      (flywire-session-enable-events
       session '(:events (:step-end :exec-complete :exec-cancelled :minibuffer-open :idle))))))

(defun flywire-async--detach-session (session)
  "Remove handlers and disable events for SESSION."
  (when-let ((handler (gethash session flywire-async--session-handlers)))
    (flywire-session-off-event session handler)
    (remhash session flywire-async--session-handlers))
  (flywire-session-disable-events session))

;;;###autoload
(define-minor-mode flywire-async-mode
  "Toggle asynchronous monitoring of Emacs state."
  :global t
  :group 'flywire
  (let ((session (flywire-session-current)))
    (if flywire-async-mode
        (flywire-async--attach-session session)
      (flywire-async--detach-session session))))

(provide 'flywire-async)
;;; flywire-async.el ends here

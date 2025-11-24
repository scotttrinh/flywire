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
(require 'json)

(defcustom flywire-async-output-handler #'flywire-async-default-output-handler
  "Function to call with the snapshot plist when state changes.
The function receives a single argument: the snapshot plist."
  :type 'function
  :group 'flywire)

(defun flywire-async-default-output-handler (snapshot)
  "Default handler: print SNAPSHOT as JSON to stdout."
  (message "EMACS_DRIVER_SNAPSHOT:%s" (json-encode snapshot)))

(defvar flywire-async--idle-timer nil)

(defun flywire-async--on-minibuffer-setup ()
  "Called when minibuffer opens."
  ;; Slight delay to ensure minibuffer content is ready?
  ;; Usually setup hook is run when buffer is ready.
  (funcall flywire-async-output-handler (flywire-snapshot-get-snapshot)))

(defun flywire-async--on-idle ()
  "Called when Emacs is idle."
  (funcall flywire-async-output-handler (flywire-snapshot-get-snapshot)))

(defun flywire-async--session-adapter (event)
  "Forward session EVENT to the output handler."
  (let ((type (plist-get event :type)))
    ;; Forward interesting events
    (when (memq type '(:step-end :exec-complete :minibuffer-open :idle))
      (let ((snap (or (plist-get event :snapshot)
                      ;; For events that imply state change but might not have a snapshot attached yet
                      (and (memq type '(:minibuffer-open :idle))
                           (flywire-snapshot-get-snapshot)))))
        (when snap
          (funcall flywire-async-output-handler snap))))))

(defun flywire-async--enable-monitoring ()
  "Setup hooks and timers."
  (add-hook 'minibuffer-setup-hook #'flywire-async--on-minibuffer-setup)
  ;; Cancel existing timer if any
  (when flywire-async--idle-timer (cancel-timer flywire-async--idle-timer))
  ;; Set new idle timer (e.g., 0.5s)
  (setq flywire-async--idle-timer
        (run-with-idle-timer 0.5 t #'flywire-async--on-idle))
  ;; Attach to current session
  (when (fboundp 'flywire-session-current)
    (flywire-session-on-event (flywire-session-current) #'flywire-async--session-adapter)))

(defun flywire-async--disable-monitoring ()
  "Teardown hooks and timers."
  (remove-hook 'minibuffer-setup-hook #'flywire-async--on-minibuffer-setup)
  (when flywire-async--idle-timer
    (cancel-timer flywire-async--idle-timer)
    (setq flywire-async--idle-timer nil))
  ;; Note: We don't easily unregister the session handler here without more bookkeeping,
  ;; but for the default session it's acceptable to leave it or we'd need to store the lambda.
  )

;;;###autoload
(define-minor-mode flywire-async-mode
  "Toggle asynchronous monitoring of Emacs state."
  :global t
  :group 'flywire
  (if flywire-async-mode
      (flywire-async--enable-monitoring)
    (flywire-async--disable-monitoring)))

(provide 'flywire-async)
;;; flywire-async.el ends here

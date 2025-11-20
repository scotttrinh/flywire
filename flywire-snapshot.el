;;; flywire-snapshot.el --- Snapshot helpers for flywire -*- lexical-binding: t; -*-

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

;; Utilities for building a JSON serializable snapshot of the current frame
;; so an external agent can inspect Emacs state without relying on global side
;; effects.

;;; Code:

(require 'cl-lib)

(defun flywire-snapshot--safe-run (fn)
  "Run FN and return its value, returning nil if FN signals an error."
  (condition-case err
      (funcall fn)
    (error
     (message "flywire snapshot helper failed: %S" err)
     nil)))

(defun flywire-snapshot--windowed-content (&optional radius)
  "Return text around point within RADIUS lines (default 50)."
  (let ((r (or radius 50)))
    (save-excursion
      (let* ((total-lines (line-number-at-pos (point-max)))
             (current-line (line-number-at-pos))
             (start-line (max 1 (- current-line r)))
             (end-line (min total-lines (+ current-line r)))
             (start-pos (progn
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point)))
             (end-pos (progn
                        (goto-char (point-min))
                        (forward-line end-line)
                        (point))))
        (buffer-substring-no-properties start-pos end-pos)))))

(defun flywire-snapshot--build-minibuffer-state ()
  "Return plist describing the minibuffer, or nil if inactive."
  (let ((window (active-minibuffer-window)))
    (when window
      (with-current-buffer (window-buffer window)
        (list :active t
              :prompt (buffer-substring-no-properties (point-min) (minibuffer-prompt-end))
              :contents (or (minibuffer-contents) ""))))))

(defun flywire-snapshot--window-list-info ()
  "Describe the visible window configuration as a list of buffers."
  (mapcar (lambda (win)
            (with-selected-window win
              (list :name (buffer-name)
                    :file (buffer-file-name))))
          (window-list nil 0)))

;;;###autoload
(defun flywire-snapshot-get-snapshot ()
  "Return a plist describing the current frame and buffer context.
The plist is safe to JSON serialize and omits overwhelming data (only a
window of lines around point is captured)."
  (let ((buffer nil)
        buffer-info content cursor minibuffer window-config)
    (setq buffer-info
          (flywire-snapshot--safe-run
           (lambda ()
             (setq buffer (current-buffer))
             (with-current-buffer buffer
               (list :name (buffer-name)
                     :major-mode major-mode
                     :file (or buffer-file-name "")))))
          content
          (flywire-snapshot--safe-run
           (lambda ()
             (with-current-buffer buffer
               (flywire-snapshot--windowed-content))))
          cursor
          (flywire-snapshot--safe-run
           (lambda ()
             (with-current-buffer buffer
               (list :point (point)
                     :line (line-number-at-pos)
                     :column (current-column)))))
          minibuffer (flywire-snapshot--safe-run #'flywire-snapshot--build-minibuffer-state)
          window-config (flywire-snapshot--safe-run #'flywire-snapshot--window-list-info))
    (list :buffer-info buffer-info
          :content content
          :cursor cursor
          :minibuffer minibuffer
          :window-configuration window-config)))

(provide 'flywire-snapshot)
;;; flywire-snapshot.el ends here

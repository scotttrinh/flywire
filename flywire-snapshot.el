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

(defcustom flywire-snapshot-context-lines 50
  "Number of lines of context to include around the cursor."
  :type 'integer
  :group 'flywire)

(defcustom flywire-snapshot-max-line-length 1000
  "Maximum length of a line to include in the snapshot.
Lines longer than this will be truncated to avoid token exhaustion."
  :type 'integer
  :group 'flywire)

(defcustom flywire-snapshot-collectors
  '(buffer-info content cursor minibuffer window-configuration)
  "List of data collectors to run when generating a snapshot.
Valid symbols are: `buffer-info', `content', `cursor', `minibuffer',
`window-configuration'."
  :type '(set (const buffer-info)
              (const content)
              (const cursor)
              (const minibuffer)
              (const window-configuration))
  :group 'flywire)

(defun flywire-snapshot--safe-run (fn)
  "Run FN and return its value, returning nil if FN signals an error."
  (condition-case err
      (funcall fn)
    (error
     (message "flywire snapshot helper failed: %S" err)
     nil)))

(defun flywire-snapshot--windowed-content (&optional radius)
  "Return text around point within RADIUS lines.
Defaults to `flywire-snapshot-context-lines'.
Respects `flywire-snapshot-max-line-length`."
  (let ((r (or radius flywire-snapshot-context-lines))
        (max-len flywire-snapshot-max-line-length)
        (lines '()))
    (save-excursion
      (let* ((total-lines (line-number-at-pos (point-max)))
             (current-line (line-number-at-pos))
             (start-line (max 1 (- current-line r)))
             (end-line (min total-lines (+ current-line r))))
        (goto-char (point-min))
        (forward-line (1- start-line))
        (dotimes (_ (1+ (- end-line start-line)))
          (let ((line-end (line-end-position)))
            (if (and max-len (> (- line-end (point)) max-len))
                (push (concat (buffer-substring-no-properties (point) (+ (point) max-len)) "...") lines)
              (push (buffer-substring-no-properties (point) line-end) lines))
            (forward-line 1)))))
    (mapconcat #'identity (nreverse lines) "\n")))

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
window of lines around point is captured).
The content included is determined by `flywire-snapshot-collectors'."
  (let ((buffer (current-buffer))
        (result nil))
    (when (memq 'buffer-info flywire-snapshot-collectors)
      (setq result
            (plist-put result :buffer-info
                       (flywire-snapshot--safe-run
                        (lambda ()
                          (with-current-buffer buffer
                            (list :name (buffer-name)
                                  :major-mode major-mode
                                  :file (or buffer-file-name ""))))))))
    (when (memq 'content flywire-snapshot-collectors)
      (setq result
            (plist-put result :content
                       (flywire-snapshot--safe-run
                        (lambda ()
                          (with-current-buffer buffer
                            (flywire-snapshot--windowed-content)))))))
    (when (memq 'cursor flywire-snapshot-collectors)
      (setq result
            (plist-put result :cursor
                       (flywire-snapshot--safe-run
                        (lambda ()
                          (with-current-buffer buffer
                            (list :point (point)
                                  :line (line-number-at-pos)
                                  :column (current-column))))))))
    (when (memq 'minibuffer flywire-snapshot-collectors)
      (setq result
            (plist-put result :minibuffer
                       (flywire-snapshot--safe-run #'flywire-snapshot--build-minibuffer-state))))
    (when (memq 'window-configuration flywire-snapshot-collectors)
      (setq result
            (plist-put result :window-configuration
                       (flywire-snapshot--safe-run #'flywire-snapshot--window-list-info))))
    result))

(provide 'flywire-snapshot)
;;; flywire-snapshot.el ends here

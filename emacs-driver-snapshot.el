;;; emacs-driver-snapshot.el --- Snapshot helpers for emacs-driver -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for building a JSON serializable snapshot of the current frame
;; so an external agent can inspect Emacs state without relying on global side
;; effects.

;;; Code:

(require 'cl-lib)

(defun emacs-driver--safe-run (fn)
  "Run FN and return its value, returning nil if FN signals an error."
  (condition-case err
      (funcall fn)
    (error
     (message "emacs-driver snapshot helper failed: %S" err)
     nil)))

(defun emacs-driver--windowed-content (&optional radius)
  "Return text around point within RADIUS lines (default 50)."
  (let ((radius (or radius 50)))
    (save-excursion
      (let* ((total-lines (line-number-at-pos (point-max)))
             (current-line (line-number-at-pos))
             (start-line (max 1 (- current-line radius)))
             (end-line (min total-lines (+ current-line radius)))
             (start-pos (progn
                          (goto-char (point-min))
                          (forward-line (1- start-line))
                          (point)))
             (end-pos (progn
                        (goto-char (point-min))
                        (forward-line end-line)
                        (point))))
        (buffer-substring-no-properties start-pos end-pos)))))

(defun emacs-driver--build-minibuffer-state ()
  "Return plist describing the minibuffer, or nil if inactive."
  (let ((window (active-minibuffer-window)))
    (when window
      (with-current-buffer (window-buffer window)
        (list :active t
              :prompt (buffer-substring-no-properties (point-min) (minibuffer-prompt-end))
              :contents (or (minibuffer-contents) ""))))))

(defun emacs-driver--window-list-info ()
  "Describe the visible window configuration as a list of buffers."
  (mapcar (lambda (win)
            (with-selected-window win
              (list :name (buffer-name)
                    :file (buffer-file-name))))
          (window-list nil 0)))

;;;###autoload
(defun emacs-driver-get-snapshot ()
  "Return a plist describing the current frame and buffer context.
The plist is safe to JSON serialize and omits overwhelming data (only a
window of lines around point is captured)."
  (let ((buffer nil)
        buffer-info content cursor minibuffer window-config)
    (setq buffer-info
          (emacs-driver--safe-run
           (lambda ()
             (setq buffer (current-buffer))
             (with-current-buffer buffer
               (list :name (buffer-name)
                     :major-mode major-mode
                     :file (or buffer-file-name "")))))
          content
          (emacs-driver--safe-run
           (lambda ()
             (with-current-buffer buffer
               (emacs-driver--windowed-content))))
          cursor
          (emacs-driver--safe-run
           (lambda ()
             (with-current-buffer buffer
               (list :point (point)
                     :line (line-number-at-pos)
                     :column (current-column)))))
          minibuffer (emacs-driver--safe-run #'emacs-driver--build-minibuffer-state)
          window-config (emacs-driver--safe-run #'emacs-driver--window-list-info))
    (list :buffer-info buffer-info
          :content content
          :cursor cursor
          :minibuffer minibuffer
          :window-configuration window-config)))

(provide 'emacs-driver-snapshot)
;;; emacs-driver-snapshot.el ends here

;;; test/emacs-driver-test.el --- Basic sanity checks for emacs-driver -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensure the package can be loaded in batch and returns sensible values from
;; its snapshot API.

;;; Code:

(require 'emacs-driver)

(ert-deftest emacs-driver-snapshot-structure ()
  "Snapshot should return expected structure."
  (let ((snapshot (emacs-driver-get-snapshot)))
    (should (plist-member snapshot :buffer-info))
    (should (plist-member snapshot :cursor))
    (should (plist-member snapshot :content))
    (should (plist-member snapshot :minibuffer))
    (should (plist-member snapshot :window-configuration))))

(ert-deftest emacs-driver-snapshot-content-windowing ()
  "Snapshot should window content around cursor."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
    (goto-char (point-min))
    (forward-line 2) ; On Line 3
    (let* ((snapshot (emacs-driver-get-snapshot))
           (content (plist-get snapshot :content)))
      (should (string-search "Line 3" content))
      ;; Verify we got the whole buffer since it's small
      (should (string-search "Line 1" content))
      (should (string-search "Line 5" content)))))

(ert-deftest emacs-driver-snapshot-content-truncation ()
  "Snapshot should truncate large buffers."
  (with-temp-buffer
    ;; Insert 200 lines
    (dotimes (i 200)
      (insert (format "Line %d\n" i)))
    (goto-char (point-min))
    (forward-line 100) ; Middle of buffer
    (let* ((snapshot (emacs-driver-get-snapshot))
           (content (plist-get snapshot :content)))
      ;; Should have the current line
      (should (string-search "Line 100" content))
      ;; Should NOT have the beginning or end (default radius is 50)
      (should-not (string-search "Line 0" content))
      (should-not (string-search "Line 199" content)))))

(ert-deftest emacs-driver-action-queue-keys ()
  "Simulating keys should push to unread-command-events."
  (setq unread-command-events nil)
  (emacs-driver-simulate-keys "C-x C-f")
  ;; C-x is 24, C-f is 6. kbd converts string to list of events.
  (should (equal unread-command-events (listify-key-sequence (kbd "C-x C-f")))))

(ert-deftest emacs-driver-action-queue-text ()
  "Typing text should push chars to unread-command-events."
  (setq unread-command-events nil)
  (emacs-driver-push-input "hello")
  (should (equal unread-command-events '(?h ?e ?l ?l ?o))))

(ert-deftest emacs-driver-do-integration ()
  "Driver should parse JSON and execute actions."
  (setq unread-command-events nil)
  (let ((json "[{\"action\": \"type\", \"text\": \"foo\"}, {\"action\": \"key\", \"chord\": \"RET\"}]"))
    (emacs-driver-do json)
    (let ((expected (append '(?f ?o ?o) (listify-key-sequence (kbd "RET")))))
      (should (equal unread-command-events expected)))))

(provide 'test/emacs-driver-test)
;;; test/emacs-driver-test.el ends here

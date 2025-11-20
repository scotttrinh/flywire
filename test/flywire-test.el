;;; test/flywire-test.el --- Basic sanity checks for flywire -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensure the package can be loaded in batch and returns sensible values from
;; its snapshot API.

;;; Code:

(require 'flywire)

(ert-deftest flywire-snapshot-structure ()
  "Snapshot should return expected structure."
  (let ((snapshot (flywire-snapshot-get-snapshot)))
    (should (plist-member snapshot :buffer-info))
    (should (plist-member snapshot :cursor))
    (should (plist-member snapshot :content))
    (should (plist-member snapshot :minibuffer))
    (should (plist-member snapshot :window-configuration))))

(ert-deftest flywire-snapshot-content-windowing ()
  "Snapshot should window content around cursor."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
    (goto-char (point-min))
    (forward-line 2) ; On Line 3
    (let* ((snapshot (flywire-snapshot-get-snapshot))
           (content (plist-get snapshot :content)))
      (should (string-search "Line 3" content))
      ;; Verify we got the whole buffer since it's small
      (should (string-search "Line 1" content))
      (should (string-search "Line 5" content)))))

(ert-deftest flywire-snapshot-content-truncation ()
  "Snapshot should truncate large buffers."
  (with-temp-buffer
    ;; Insert 200 lines
    (dotimes (i 200)
      (insert (format "Line %d\n" i)))
    (goto-char (point-min))
    (forward-line 100) ; Middle of buffer
    (let* ((snapshot (flywire-snapshot-get-snapshot))
           (content (plist-get snapshot :content)))
      ;; Should have the current line
      (should (string-search "Line 100" content))
      ;; Should NOT have the beginning or end (default radius is 50)
      (should-not (string-search "Line 0" content))
      (should-not (string-search "Line 199" content)))))

(ert-deftest flywire-action-queue-keys ()
  "Simulating keys should push to unread-command-events."
  (setq unread-command-events nil)
  (flywire-action-simulate-keys "C-x C-f")
  ;; C-x is 24, C-f is 6. kbd converts string to list of events.
  (should (equal unread-command-events (listify-key-sequence (kbd "C-x C-f")))))

(ert-deftest flywire-action-queue-text ()
  "Typing text should push chars to unread-command-events."
  (setq unread-command-events nil)
  (flywire-action-push-input "hello")
  (should (equal unread-command-events '(?h ?e ?l ?l ?o))))

(ert-deftest flywire-do-integration ()
  "Driver should parse JSON and execute actions."
  (setq unread-command-events nil)
  (let ((json "[{\"action\": \"type\", \"text\": \"foo\"}, {\"action\": \"key\", \"chord\": \"RET\"}]"))
    (flywire-do json)
    (let ((expected (append '(?f ?o ?o) (listify-key-sequence (kbd "RET")))))
      (should (equal unread-command-events expected)))))

(provide 'test/flywire-test)
;;; test/flywire-test.el ends here

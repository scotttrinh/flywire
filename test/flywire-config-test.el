;;; test/flywire-config-test.el --- Tests for configurable flywire -*- lexical-binding: t; -*-

(require 'flywire)

(ert-deftest flywire-config-snapshot-lines ()
  "Snapshot should respect flywire-snapshot-context-lines."
  (with-temp-buffer
    (dotimes (i 100)
      (insert (format "Line %d\n" i)))
    (goto-char (point-min))
    (forward-line 50)
    (let ((flywire-snapshot-context-lines 2))
      (let* ((snapshot (flywire-snapshot-get-snapshot))
             (content (plist-get snapshot :content)))
        ;; Should have current line (50) and +/- 2 lines (48-52)
        (should (string-search "Line 50" content))
        (should (string-search "Line 48" content))
        (should (string-search "Line 52" content))
        (should-not (string-search "Line 47" content))
        (should-not (string-search "Line 53" content))))))

(ert-deftest flywire-config-collectors ()
  "Snapshot should respect flywire-snapshot-collectors."
  (let ((flywire-snapshot-collectors '(cursor)))
    (let ((snapshot (flywire-snapshot-get-snapshot)))
      (should (plist-member snapshot :cursor))
      (should-not (plist-member snapshot :content))
      (should-not (plist-member snapshot :buffer-info))
      (should-not (plist-member snapshot :minibuffer))
      (should-not (plist-member snapshot :window-configuration)))))

(ert-deftest flywire-do-native-struct ()
  "flywire-do should accept native alists."
  (setq unread-command-events nil)
  (let ((instructions '(((action . "type") (text . "bar")))))
    (flywire-do instructions)
    (should (equal unread-command-events '(?b ?a ?r)))))

(provide 'test/flywire-config-test)

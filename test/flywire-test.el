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
  "Driver should execute parsed actions."
  (setq unread-command-events nil)
  (let* ((json "[{\"action\": \"type\", \"text\": \"foo\"}, {\"action\": \"key\", \"chord\": \"RET\"}]")
         (parsed (json-parse-string json :object-type 'alist :array-type 'list)))
    (flywire-do parsed)
    (let ((expected (append '(?f ?o ?o) (listify-key-sequence (kbd "RET")))))
      (should (equal unread-command-events expected)))))

(ert-deftest flywire-session-creation ()
  "Should create session with default environment."
  (let ((session (flywire-session-create)))
    (should (flywire-session-p session))
    (should (flywire-env-p (flywire-session-env session)))
    (should (equal (flywire-env-name (flywire-session-env session)) "default"))))

(ert-deftest flywire-session-exec-sync ()
  "Should execute steps synchronously and return structured result."
  (let* ((session (flywire-session-create))
         (steps `(((action . "type") (text . "hello"))))
         (result (flywire-session-exec session steps)))
    (should (eq (plist-get result :status) :ok))
    (should (listp (plist-get result :steps)))
    (should (= (length (plist-get result :steps)) 1))
    (let ((step-res (car (plist-get result :steps))))
      (should (eq (alist-get :status step-res) :ok))
      (should (equal (alist-get 'action (alist-get :action step-res)) "type")))))

(ert-deftest flywire-snapshot-profiles ()
  "Snapshot should respect profiles."
  (let ((snap-minimal (flywire-snapshot-get-snapshot 'minimal))
        (snap-default (flywire-snapshot-get-snapshot 'default)))
    ;; Minimal has buffer-info but no content
    (should (plist-member snap-minimal :buffer-info))
    (should-not (plist-member snap-minimal :content))
    (should-not (plist-member snap-minimal :window-configuration))
    
    ;; Default has content and window-configuration
    (should (plist-member snap-default :content))
    (should (plist-member snap-default :window-configuration))))

(ert-deftest flywire-snapshot-messages ()
  "Snapshot should capture messages."
  (message "Test Message 1")
  (message "Test Message 2")
  (let* ((snap (flywire-snapshot-get-snapshot nil '(messages)))
         (msgs (plist-get snap :messages)))
    (should (stringp msgs))
    (should (string-search "Test Message 1" msgs))
    (should (string-search "Test Message 2" msgs))))

(ert-deftest flywire-snapshot-visible-content ()
  "Snapshot should capture visible content."
  (with-temp-buffer
    (insert "Visible\n")
    ;; In batch mode, window manipulation might be limited, but we try.
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((snap (flywire-snapshot-get-snapshot nil '(visible-content)))
           (vis (plist-get snap :visible-content)))
      (should (string-search "Visible" vis)))))

(ert-deftest flywire-snapshot-extensions ()
  "Snapshot hook should allow extensions."
  (let ((hook-fn (lambda (s) (nconc s (list :extra "data")))))
    (add-hook 'flywire-snapshot-collect-hook hook-fn)
    (unwind-protect
        (let ((snap (flywire-snapshot-get-snapshot 'minimal)))
          (should (equal (plist-get snap :extra) "data")))
      (remove-hook 'flywire-snapshot-collect-hook hook-fn))))

(ert-deftest flywire-action-registry-defaults ()
  "Registry should contain default tools."
  (should (gethash "type_text" flywire-action-registry))
  (should (gethash "press_key" flywire-action-registry))
  (should (gethash "run_command" flywire-action-registry))
  (should (gethash "cancel" flywire-action-registry))
  (should (gethash "goto_location" flywire-action-registry)))

(ert-deftest flywire-action-cancel-execution ()
  "Cancel action should abort session execution."
  (let* ((session (flywire-session-create))
         ;; Pass "cancel" via fallback dispatch which calls flywire-action-execute-tool directly
         (steps `(((action . "cancel")))) 
         (result (flywire-session-exec session steps)))
    (should (eq (plist-get result :status) :cancelled))))

(ert-deftest flywire-action-goto-location ()
  "Goto location should move point."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3")
    (goto-char (point-min))
    (flywire-action-goto-location '(("line" . 2)))
    (should (looking-at "Line 2"))
    
    (flywire-action-goto-location '(("point" . 1)))
    (should (eq (point) 1))
    
    (flywire-action-goto-location '(("column" . 2)))
    (should (eq (current-column) 2))))

(ert-deftest flywire-context-execution ()
  "Step should run in specified buffer."
  (let ((buf1 (generate-new-buffer "buf1"))
        (buf2 (generate-new-buffer "buf2")))
    (with-current-buffer buf1 (insert "buf1"))
    (with-current-buffer buf2 (insert "buf2"))
    
    ;; Register a test tool to verify context.
    (flywire-register-action "test_context"
      (lambda (_args)
        (insert (buffer-name))))
    
    (let ((steps `(((action . "test_context") (buffer . ,(buffer-name buf2))))))
      (flywire-session-exec (flywire-session-create) steps)
      (with-current-buffer buf2
        (should (string-search "buf2" (buffer-string))))
      (with-current-buffer buf1
        (should-not (string-search "buf2" (buffer-string)))))
    
    (kill-buffer buf1)
    (kill-buffer buf2)))

(ert-deftest flywire-window-id-snapshot-and-context ()
  "Snapshot should include window ID and context should respect it."
  (let* ((snap (flywire-snapshot-get-snapshot 'default))
         (wins (plist-get snap :window-configuration))
         (first-win (car wins))
         (win-id (plist-get first-win :id))
         (win-name (plist-get first-win :name)))
    (should win-id)
    (should (string-prefix-p "win-" win-id))
    
    ;; Test context
    (flywire-register-action "test_win_context"
       (lambda (_args)
         (insert (format "WIN:%s" (flywire-snapshot--get-window-id (selected-window))))))
    
    (let ((steps `(((action . "test_win_context") (window-id . ,win-id)))))
      (flywire-session-exec (flywire-session-create) steps)
      (with-current-buffer (get-buffer win-name)
        (should (string-search (format "WIN:%s" win-id) (buffer-string)))))))

(provide 'test/flywire-test)
;;; test/flywire-test.el ends here

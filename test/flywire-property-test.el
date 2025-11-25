;;; test/flywire-property-test.el --- Property tests for flywire -*- lexical-binding: t; -*-

(require 'propcheck)
(require 'flywire)
(require 'flywire-test-helpers)

(propcheck-deftest flywire-prop-push-input ()
  "Pushing any ASCII string input should queue corresponding events."
  (let ((done #'ignore))
    (flywire-test-with-bindings done ((unread-command-events nil))
      (let ((s (propcheck-generate-string "input")))
        (flywire-action-push-input s)
        (let ((expected (string-to-list s)))
          (propcheck-should (equal unread-command-events expected))))
      (funcall done))))

(propcheck-deftest flywire-prop-simulate-keys-simple ()
  "Simulating simple alphanumeric strings should behave like typing."
  (let ((done #'ignore))
    (flywire-test-with-bindings done ((unread-command-events nil))
      (let ((s (propcheck-generate-string "keys")))
        ;; Only keep alphanumeric chars to ensure valid key sequence
        (setq s (replace-regexp-in-string "[^a-zA-Z0-9]" "" s))
        (when (not (string-empty-p s))
          (flywire-action-simulate-keys s)
          (let ((expected (listify-key-sequence (kbd s))))
            (propcheck-should (equal unread-command-events expected)))))
      (funcall done))))

(ert-deftest flywire-snapshot-content-empty-buffer ()
  "Snapshot of empty buffer should be empty string."
  (with-temp-buffer
    (let ((content (flywire-snapshot--windowed-content)))
      (should (string-equal content "")))))

(propcheck-deftest flywire-prop-snapshot-content-substring ()
  "Windowed content should always be a valid substring of the full buffer."
  (let* ((radius (propcheck-generate-integer "radius" :min 1 :max 20))
         (lines (propcheck-generate-proper-list "lines" 
                  :value-fn (lambda (_ignored) (propcheck-generate-string "line"))))
         (full-text (mapconcat #'identity lines "\n")))
    
    (with-temp-buffer
      (insert full-text)
      (let ((max-pos (point-max)))
        (goto-char (if (= max-pos 1)
                       1
                     (propcheck-generate-integer "point" :min 1 :max max-pos)))
        
        (let ((content (flywire-snapshot--windowed-content radius)))
          ;; The snapshot content must be present in the full buffer
          (propcheck-should (string-search content full-text)))))))

(propcheck-deftest flywire-prop-windowed-content-radius ()
  "Windowed content should respect the radius constraint."
  (let* ((radius (propcheck-generate-integer "radius" :min 1 :max 10))
         (lines (propcheck-generate-proper-list "lines"
                  :value-fn (lambda (_ignored) (propcheck-generate-string "line"))))
         (full-text (mapconcat #'identity lines "\n")))
    
    (with-temp-buffer
      (insert full-text)
      (let ((max-pos (point-max)))
        (goto-char (if (= max-pos 1) 1
                     (propcheck-generate-integer "point" :min 1 :max max-pos)))
        
        (let ((content (flywire-snapshot--windowed-content radius)))
           (let ((num-lines (with-temp-buffer
                              (insert content)
                              (count-lines (point-min) (point-max)))))
             (propcheck-should (<= num-lines (1+ (* 2 radius))))))))))

(propcheck-deftest flywire-prop-minibuffer-state ()
  "Minibuffer state construction should correctly split prompt and content."
  (let ((prompt (propcheck-generate-string "prompt"))
        (content (propcheck-generate-string "content")))
    
    (with-temp-buffer
      (let ((buf (current-buffer)))
        ;; We insert prompt + content to simulate the buffer state
        (insert prompt content)
        
        ;; Mock infrastructure to simulate minibuffer activation
        (cl-letf (((symbol-function 'active-minibuffer-window)
                   (lambda () 'fake-window))
                  ((symbol-function 'window-buffer)
                   (lambda (w) (if (eq w 'fake-window) buf (current-buffer))))
                  ((symbol-function 'minibuffer-prompt-end)
                   (lambda () (1+ (length prompt))))
                  ((symbol-function 'minibuffer-contents)
                   (lambda () content)))
           
           (let ((state (flywire-snapshot--build-minibuffer-state)))
             (propcheck-should state)
             (propcheck-should (equal (plist-get state :active) t))
             (propcheck-should (string-equal (plist-get state :prompt) prompt))
             (propcheck-should (string-equal (plist-get state :contents) content))))))))

(propcheck-deftest flywire-prop-do-json-instructions ()
  "flywire-do should execute actions from parsed JSON."
  (let ((txt (propcheck-generate-string "text")))
    (setq unread-command-events nil)
    (let* ((instruction `(("action" . "type") ("text" . ,txt)))
           (json-str (json-encode (vector instruction)))
           (parsed (json-parse-string json-str :object-type 'alist :array-type 'list)))
       ;; Mock snapshot to avoid side effects or complex return values
       (cl-letf (((symbol-function 'flywire-snapshot-get-snapshot) (lambda (&rest _args) '(:mock-snapshot t))))
         (let ((result (flywire-do parsed)))
           (propcheck-should (equal result '(:mock-snapshot t)))
           (propcheck-should (equal unread-command-events (string-to-list txt))))))))

(propcheck-deftest flywire-prop-simulate-keys-robustness ()
  "Simulating invalid keys should not crash."
  (let ((s (propcheck-generate-string "invalid-keys")))
    (setq unread-command-events nil)
    ;; This should catch errors internally and return nil or partial events
    (flywire-action-simulate-keys s)
    ;; We just assert we reached here without signal
    (propcheck-should t)))

(propcheck-deftest flywire-prop-do-alist-instructions ()
  "flywire-do should accept alist instructions and execute actions."
  (let ((txt (propcheck-generate-string "text")))
    (setq unread-command-events nil)
    (let* ((instruction `(("action" . "type") ("text" . ,txt)))
           (instructions (list instruction)))
       ;; Mock snapshot to avoid side effects or complex return values
       (cl-letf (((symbol-function 'flywire-snapshot-get-snapshot) (lambda (&rest _args) '(:mock-snapshot t))))
         (let ((result (flywire-do instructions)))
           (propcheck-should (equal result '(:mock-snapshot t)))
           (propcheck-should (equal unread-command-events (string-to-list txt))))))))

(propcheck-deftest flywire-prop-do-alist-multiple-actions ()
  "flywire-do should execute multiple actions from an alist."
  (let ((txt1 (propcheck-generate-string "text1"))
        (txt2 (propcheck-generate-string "text2")))
    (setq unread-command-events nil)
    (let* ((instructions `((("action" . "type") ("text" . ,txt1))
                           (("action" . "type") ("text" . ,txt2)))))
       ;; Mock snapshot
       (cl-letf (((symbol-function 'flywire-snapshot-get-snapshot) (lambda (&rest _args) '(:mock-snapshot t))))
         (let ((result (flywire-do instructions)))
           (unless (equal result '(:mock-snapshot t))
             (message "Result mismatch: %S" result))
           (let ((expected (append (string-to-list txt1) (string-to-list txt2))))
             (unless (equal unread-command-events expected)
               (message "Events mismatch. txt1: %S, txt2: %S. Expected: %S, Got: %S" txt1 txt2 expected unread-command-events))
             (propcheck-should (equal result '(:mock-snapshot t)))
             (propcheck-should (equal unread-command-events expected))))))))

(propcheck-deftest flywire-prop-do-alist-mixed-keys-and-text ()
  "flywire-do should handle mixed key and text actions in an alist."
  (let ((txt (propcheck-generate-string "text")))
    ;; Only keep alphanumeric chars for simple key simulation
    (setq txt (replace-regexp-in-string "[^a-zA-Z0-9]" "" txt))
    (when (not (string-empty-p txt))
      (setq unread-command-events nil)
      (let* ((instructions `((("action" . "type") ("text" . ,txt))
                             (("action" . "key") ("chord" . "RET")))))
         ;; Mock snapshot
         (cl-letf (((symbol-function 'flywire-snapshot-get-snapshot) (lambda (&rest _args) '(:mock-snapshot t))))
           (let ((result (flywire-do instructions)))
             (unless (equal result '(:mock-snapshot t))
               (message "Result mismatch: %S" result))
             (let ((expected (append (string-to-list txt) (listify-key-sequence (kbd "RET")))))
               (unless (equal unread-command-events expected)
                 (message "Events mismatch. Text: %S. Expected: %S, Got: %S" txt expected unread-command-events))
               (propcheck-should (equal result '(:mock-snapshot t)))
               (propcheck-should (equal unread-command-events expected)))))))))

(propcheck-deftest flywire-prop-do-alist-invalid-action ()
  "flywire-do should signal error on invalid action type."
  (let ((invalid-action (propcheck-generate-string "invalid-action")))
    ;; Ensure string isn't one of the valid actions
    (while (member invalid-action '("type" "key" "command"))
      (setq invalid-action (propcheck-generate-string "invalid-action")))
    
    (let* ((instruction `(("action" . ,invalid-action)))
           (instructions (list instruction)))
      (cl-letf (((symbol-function 'flywire-snapshot-get-snapshot) (lambda (&rest _args) '(:mock-snapshot t))))
        (condition-case err
            (progn
              (flywire-do instructions)
              (message "Did not error for invalid action: %S" invalid-action)
              (propcheck-should nil)) ;; Fail if no error
          (error
           (propcheck-should t)))))))

(propcheck-deftest flywire-prop-do-json-invalid-action ()
  "flywire-do should signal error on invalid action type in parsed JSON."
  (let ((invalid-action (propcheck-generate-string "invalid-action")))
    ;; Ensure string isn't one of the valid actions
    (while (member invalid-action '("type" "key" "command"))
      (setq invalid-action (propcheck-generate-string "invalid-action")))
    
    (let* ((instruction `(("action" . ,invalid-action)))
           (json-str (json-encode (vector instruction)))
           (parsed (json-parse-string json-str :object-type 'alist :array-type 'list)))
      (cl-letf (((symbol-function 'flywire-snapshot-get-snapshot) (lambda (&rest _args) '(:mock-snapshot t))))
        (condition-case err
            (progn
              (flywire-do parsed)
              (message "Did not error for invalid action: %S" invalid-action)
              (propcheck-should nil)) ;; Fail if no error
          (error
           (propcheck-should t)))))))

(propcheck-deftest flywire-prop-do-json-invalid-syntax ()
  "Parsing malformed JSON should signal error before reaching flywire."
  (let ((garbage (propcheck-generate-string "garbage")))
    ;; Ensure it's not coincidentally valid JSON
    (while (ignore-errors (json-parse-string garbage) t)
      (setq garbage (concat "{" (propcheck-generate-string "garbage"))))
    
    (should-error (json-parse-string garbage :object-type 'alist :array-type 'list) :type 'json-error)))

(provide 'test/flywire-property-test)
;;; test/flywire-property-test.el ends here

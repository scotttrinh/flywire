;;; test/flywire-property-test.el --- Property tests for flywire -*- lexical-binding: t; -*-

(require 'propcheck)
(require 'flywire)

(propcheck-deftest flywire-prop-push-input ()
  "Pushing any ASCII string input should queue corresponding events."
  (let ((s (propcheck-generate-string "input")))
    (setq unread-command-events nil)
    (flywire-action-push-input s)
    (let ((expected (string-to-list s)))
      (propcheck-should (equal unread-command-events expected)))))

(propcheck-deftest flywire-prop-simulate-keys-simple ()
  "Simulating simple alphanumeric strings should behave like typing."
  (let ((s (propcheck-generate-string "keys")))
    ;; Only keep alphanumeric chars to ensure valid key sequence
    (setq s (replace-regexp-in-string "[^a-zA-Z0-9]" "" s))
    (when (not (string-empty-p s))
      (setq unread-command-events nil)
      (flywire-action-simulate-keys s)
      (let ((expected (listify-key-sequence (kbd s))))
        (propcheck-should (equal unread-command-events expected))))))

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
  "flywire-do should parse JSON and execute actions."
  (let ((txt (propcheck-generate-string "text")))
    (setq unread-command-events nil)
    (let* ((instruction `((action . "type") (text . ,txt)))
           (json-str (json-encode (vector instruction))))
       ;; Mock snapshot to avoid side effects or complex return values
       (cl-letf (((symbol-function 'flywire-snapshot-get-snapshot) (lambda () '(:mock-snapshot t))))
         (let ((result (flywire-do json-str)))
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

(provide 'test/flywire-property-test)

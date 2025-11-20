;;; test/emacs-driver-e2e-test.el --- End-to-end tests spawning Emacs -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)

(defun emacs-driver-e2e-get-root ()
  "Return the project root directory."
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-driver-e2e-run (eval-form)
  "Run an Emacs subprocess evaluating EVAL-FORM.
Returns the stdout as a string."
  (let* ((root (emacs-driver-e2e-get-root))
         (default-directory root)
         (emacs-bin (expand-file-name invocation-name invocation-directory))
         (output-buffer (generate-new-buffer " *e2e-output*")))
    (unwind-protect
        (let ((exit-code
               (call-process emacs-bin nil output-buffer nil
                             "-Q" "--batch"
                             "-L" "."
                             "-l" "emacs-driver.el"
                             "--eval" eval-form)))
          (unless (zerop exit-code)
            (error "Emacs subprocess failed with exit code %d. Output:\n%s"
                   exit-code (with-current-buffer output-buffer (buffer-string))))
          (with-current-buffer output-buffer (buffer-string)))
      (kill-buffer output-buffer))))

(ert-deftest emacs-driver-e2e-json-roundtrip ()
  "Launch a subprocess, send a JSON instruction, and parse the result."
  (let* ((json-arg "[{\"action\": \"type\", \"text\": \"e2e\"}]")
         ;; We use princ to output the raw JSON string to stdout
         (eval-form (format "(princ (json-encode (emacs-driver-do %S)))" json-arg))
         (output (emacs-driver-e2e-run eval-form))
         (json-res (json-parse-string output :object-type 'plist)))
    
    ;; Verify we got a valid snapshot back
    (should (plist-get json-res :buffer-info))
    (should (equal (plist-get (plist-get json-res :buffer-info) :name) "*scratch*"))
    ;; Verify content field exists (even if empty/default)
    (should (plist-get json-res :content))))

(provide 'test/emacs-driver-e2e-test)

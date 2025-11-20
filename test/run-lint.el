;;; test/run-lint.el --- Batch linter for emacs-driver  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Checks for coding style (checkdoc), packaging issues (package-lint),
;; and compilation errors (byte-compile).

;;; Code:

(require 'seq)
(require 'checkdoc)
(require 'package-lint)
(require 'bytecomp)

(defvar emacs-driver-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the emacs-driver repository root.")

(defvar emacs-driver-files
  (directory-files emacs-driver-root t "^emacs-driver.*\\.el$")
  "List of emacs-driver source files to lint.")

(defvar lint-errors nil
  "List of error messages collected during linting.")

(defun report-error (fmt &rest args)
  "Format an error message and add it to `lint-errors`."
  (let ((msg (apply #'format fmt args)))
    (push msg lint-errors)
    (message "ERROR: %s" msg)))

;; 1. Checkdoc
(message "Running checkdoc...")
(dolist (file emacs-driver-files)
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (setq buffer-file-name file)
    (let ((checkdoc-errors nil))
      (let ((checkdoc-create-error-function
             (lambda (text start end &optional unfixable)
               (push (format "%s:%d: %s" 
                             (file-name-nondirectory file)
                             (line-number-at-pos start)
                             text)
                     checkdoc-errors)
               nil)))
        (checkdoc-current-buffer t))
      (when checkdoc-errors
        (report-error "Checkdoc failed for %s:" (file-name-nondirectory file))
        (dolist (err (nreverse checkdoc-errors))
          (message "  %s" err))))))

;; 2. Package-lint
(message "Running package-lint...")
(dolist (file emacs-driver-files)
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (setq buffer-file-name file)
    (let ((errors (package-lint-buffer)))
      (when errors
        (report-error "Package-lint failed for %s:" (file-name-nondirectory file))
        (dolist (err errors)
          ;; package-lint returns a list of objects (line col type message)
          (let ((line (nth 0 err))
                (col (nth 1 err))
                (type (nth 2 err))
                (msg (nth 3 err)))
            (message "  %s:%d:%d: %s: %s" 
                     (file-name-nondirectory file)
                     line col type msg)))))))

;; 3. Byte-compile
(message "Running byte-compile...")
(setq byte-compile-error-on-warn t)
(add-to-list 'load-path emacs-driver-root)

;; Create a separate directory for byte-compiled files to avoid polluting the source tree
(defvar byte-compile-dest (make-temp-file "emacs-driver-byte-compile" t))

(dolist (file emacs-driver-files)
  (let ((dest (expand-file-name (concat (file-name-nondirectory file) "c") byte-compile-dest)))
    (condition-case err
        (unless (byte-compile-file file)
          (report-error "Byte-compilation failed for %s" (file-name-nondirectory file)))
      (error
       (report-error "Byte-compilation error for %s: %s" (file-name-nondirectory file) err)))))

;; Cleanup
(delete-directory byte-compile-dest t)

;; Final Report
(if lint-errors
    (progn
      (message "\nLinting FAILED with %d errors." (length lint-errors))
      (kill-emacs 1))
  (message "\nLinting PASSED.")
  (kill-emacs 0))

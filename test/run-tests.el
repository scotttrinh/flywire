;;; test/run-tests.el --- Batch test runner with dependencies  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Loads every *-test.el file and executes the ERT suite.
;; Dependencies (ert-async, dash, s, propcheck) are assumed to be in the load-path
;; (e.g. provided by the Nix environment).

;;; Code:

(require 'seq)

(defconst emacs-driver-test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the emacs-driver repository root.")

;; Add the project root to load-path
(add-to-list 'load-path emacs-driver-test-root)

;; Require dependencies
(require 'ert-async)
(require 'dash)
(require 's)
(require 'propcheck)

(require 'ert)

(defconst emacs-driver-test-directory
  (expand-file-name "test" emacs-driver-test-root))

;; Load all test files
(dolist (file (directory-files emacs-driver-test-directory t "-test\\.el\\'"))
  (load file nil nil t))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here

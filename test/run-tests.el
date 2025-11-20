;;; test/run-tests.el --- Runner for emacs-driver ERT tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple batch runner that bootstraps the repo and executes every test
;; defined under test/.

;;; Code:

(let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name script-dir)))
       (vendor (expand-file-name "test/vendor" root)))
  (add-to-list 'load-path root)             ; so `require` finds emacs-driver
  (add-to-list 'load-path (expand-file-name "test" root))

  ;; Add vendor paths
  (add-to-list 'load-path (expand-file-name "dash" vendor))
  (add-to-list 'load-path (expand-file-name "s" vendor))
  (add-to-list 'load-path (expand-file-name "propcheck" vendor))

  (require 'ert)

  (dolist (file (directory-files (expand-file-name "test" root) t "-test\\.el$"))
    (load file nil t))

  (ert-run-tests-batch-and-exit))

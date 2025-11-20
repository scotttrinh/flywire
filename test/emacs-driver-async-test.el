;;; test/emacs-driver-async-test.el --- Async tests for emacs-driver -*- lexical-binding: t; -*-

(require 'emacs-driver)
(require 'emacs-driver-async)

(ert-deftest emacs-driver-async-do-returns-started ()
  "Async execution should return 'started and schedule timer."
  (should (eq (emacs-driver-do-async "[]") 'started)))

(ert-deftest emacs-driver-async-execution ()
  "Async execution should eventually run steps."
  (setq unread-command-events nil)
  (emacs-driver-do-async "[{\"action\": \"type\", \"text\": \"async\"}]")
  ;; Wait for timer to run. In batch mode, sit-for should run timers.
  (sit-for 0.2)
  (should (equal unread-command-events '(?a ?s ?y ?n ?c))))

(ert-deftest emacs-driver-async-minibuffer-monitor ()
  "Minibuffer setup should trigger output handler."
  (let ((called nil))
    (cl-letf (((symbol-function 'emacs-driver-default-output-handler)
               (lambda (snapshot) (setq called t))))
      (emacs-driver-async-mode 1)
      (run-hooks 'minibuffer-setup-hook)
      (should called)
      (emacs-driver-async-mode -1))))

(ert-deftest emacs-driver-async-idle-monitor ()
  "Idle monitor should be registered."
  (emacs-driver-async-mode 1)
  ;; We look for a timer that calls emacs-driver--on-idle
  (let ((found nil))
    (dolist (timer timer-idle-list)
      (when (eq (timer--function timer) #'emacs-driver--on-idle)
        (setq found t)))
    (should found))
  (emacs-driver-async-mode -1))

(provide 'test/emacs-driver-async-test)

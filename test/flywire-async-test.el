;;; test/flywire-async-test.el --- Async tests for flywire -*- lexical-binding: t; -*-

(require 'flywire)
(require 'flywire-async)
(require 'ert-async)

(ert-deftest flywire-async-do-returns-started ()
  "Async execution should return 'started and schedule timer."
  (should (eq (flywire-do-async '()) 'started)))

(ert-deftest-async flywire-async-execution (done)
  "Async execution should eventually run steps."
  (setq unread-command-events nil)
  (let* ((json-str "[{\"action\": \"type\", \"text\": \"async\"}]")
         (parsed (json-parse-string json-str :object-type 'alist :array-type 'list)))
    (flywire-do-async parsed))
  ;; Wait for timer to run.
  (run-with-timer 0.2 nil
                  (lambda ()
                    (should (equal unread-command-events '(?a ?s ?y ?n ?c)))
                    (funcall done))))

(ert-deftest flywire-async-minibuffer-monitor ()
  "Minibuffer setup should trigger output handler."
  (let ((called nil))
    (cl-letf (((symbol-function 'flywire-async-default-output-handler)
               (lambda (snapshot) (setq called t))))
      (flywire-async-mode 1)
      (run-hooks 'minibuffer-setup-hook)
      (should called)
      (flywire-async-mode -1))))

(ert-deftest flywire-async-idle-monitor ()
  "Idle monitor should be registered."
  (flywire-async-mode 1)
  ;; We look for a timer that calls flywire-async--on-idle
  (let ((found nil))
    (dolist (timer timer-idle-list)
      (when (eq (timer--function timer) #'flywire-async--on-idle)
        (setq found t)))
    (should found))
  (flywire-async-mode -1))

(provide 'test/flywire-async-test)

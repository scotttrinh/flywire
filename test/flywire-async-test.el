;;; test/flywire-async-test.el --- Async tests for flywire -*- lexical-binding: t; -*-

(require 'flywire)
(require 'flywire-async)
(require 'flywire-test-helpers)
(require 'ert-async)

(defvar flywire-session--default-event-state)

(ert-deftest flywire-async-do-returns-started ()
  "Async execution should return 'started and schedule timer."
  (let ((handle (flywire-do-async '())))
    (should (flywire-session-async-handle-p handle))
    (should (eq (flywire-session-async-handle-status handle) :scheduled))))

(ert-deftest-async flywire-async-execution (done)
  "Async execution should eventually run steps."
  (flywire-test-with-bindings done
      ((unread-command-events nil))
    (let* ((json-str "[{\"action\": \"type\", \"text\": \"async\"}]")
           (parsed (json-parse-string json-str :object-type 'alist :array-type 'list)))
      (flywire-do-async parsed))
    ;; Wait for timer to run.
    (run-with-timer 0.2 nil
                    (lambda ()
                      (should (equal unread-command-events '(?a ?s ?y ?n ?c)))
                      (funcall done)))))

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
  (let* ((session (flywire-session-current))
         (state (and (boundp 'flywire-session--default-event-state)
                     (gethash session flywire-session--default-event-state))))
    (should (timerp (plist-get state :idle-timer))))
  (flywire-async-mode -1))

(ert-deftest-async flywire-session-async-events (done)
  "Should emit events during async execution."
  (flywire-test-with-bindings done ()
    (let* ((session (flywire-session-create :id 'async-test))
           (steps `(((action . "type") (text . "foo"))))
           (events nil))
      (flywire-session-on-event session (lambda (e) (push e events)))
      (let ((handle (flywire-session-start-async session steps)))
        (should (flywire-session-async-handle-p handle)))
      
      (run-with-timer 0.2 nil
                      (lambda ()
                        (should (>= (length events) 4)) ;; exec-start, step-start, step-end, exec-complete
                        (let ((types (mapcar (lambda (e) (plist-get e :type)) events)))
                          (should (member :exec-start types))
                          (should (member :step-start types))
                          (should (member :step-end types))
                          (should (member :exec-complete types)))
                        (funcall done))))))

(ert-deftest flywire-session-cancel-handle ()
  "Cancelling an async handle should update status and emit events."
  (let* ((session (flywire-session-create :id 'cancel-test))
         (events nil))
    (flywire-session-on-event session (lambda (e) (push e events)))
    (let ((handle (flywire-session-start-async session '(((action . "type") (text . "abc"))))))
      (flywire-session-cancel handle "stop")
      (should (eq (flywire-session-async-handle-status handle) :cancelled))
      (should-not (timerp (flywire-session-async-handle-timer handle)))
      (should (equal (plist-get (flywire-session-async-handle-result handle) :status) :cancelled))
      (let ((types (mapcar (lambda (e) (plist-get e :type)) events)))
        (should (member :exec-cancelled types))
        (should (member :exec-complete types))))))

(provide 'test/flywire-async-test)

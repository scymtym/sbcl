(cl:in-package :run-tests)

(defvar *break-on-error*) ; TODO duplicated in run-tests

(defun run (file test-fun
            break-on-failure break-on-expected-failure break-on-error
            interpret)
  (setf *break-on-failure* break-on-failure
        *break-on-expected-failure* break-on-expected-failure
        *break-on-error* break-on-error)
  (when interpret
    (setf *test-evaluator-mode* :interpret)
    (push :interpreter *features*))
  (setf sb-ext:*evaluator-mode* *test-evaluator-mode*)
  (format t "// Running ~a in ~a evaluator mode~%"
          (enough-namestring file) *evaluator-mode*)
  (restart-case
      (handler-bind
          ((error (lambda (condition)
                    (push (test-util::unhandled-error file 'dummy 0 (let ((*print-readably* nil))
                                                                      (princ-to-string condition)))
                          test-util::*results*)
                    (cond (*break-on-error*
                           (test-util:really-invoke-debugger condition))
                          (t
                           (format *error-output* "~&Unhandled ~a: ~a~%"
                                   (type-of condition) condition)
                           (sb-debug:print-backtrace)))
                    (invoke-restart 'skip-file))))
        (let ((*package* (find-package :cl-user)))
          (funcall test-fun file)))
    (skip-file ()
      (format t ">>>~a<<<~%" test-util::*results*)))
  (report-test-status)
  (exit :code 104))

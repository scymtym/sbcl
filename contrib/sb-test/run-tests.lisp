(cl:in-package "RUN-TESTS")

(defvar *all-results* nil)
(defvar *break-on-error* nil)
(defvar *report-skipped-tests* nil)
(defvar *explicit-test-files* nil)

(defun run-all ()
  (loop :with remainder = (rest *posix-argv*)
     :while remainder
     :for arg = (pop remainder)
     :do (cond
           ((string= arg "--evaluator-mode")
            (let ((mode (pop remainder)))
              (cond
                ((string= mode "interpret")
                 (setf *test-evaluator-mode* :interpret))
                ((string= mode "compile")
                 (setf *test-evaluator-mode* :compile))
                (t
                 (error "~@<Invalid evaluator mode: ~A. Must be one ~
                           of interpret, compile.~@:>"
                        mode)))))
           ((string= arg "--break-on-failure")
            (setf *break-on-error* t)
            (setf test-util:*break-on-failure* t))
           ((string= arg "--break-on-expected-failure")
            (setf test-util:*break-on-expected-failure* t))
           ((string= arg "--report-skipped-tests")
            (setf *report-skipped-tests* t))
           ((string= arg "--no-color"))
           (t
            (push (truename (parse-namestring arg)) *explicit-test-files*))))
  (setf *explicit-test-files* (nreverse *explicit-test-files*))
  (pure-runner (pure-load-files) 'load-test)
  (pure-runner (pure-cload-files) 'cload-test)
  (impure-runner (impure-load-files) 'load-test)
  (impure-runner (impure-cload-files) 'cload-test)
  #-win32 (impure-runner (sh-files) 'sh-test)
  (test-util::report *all-results* :report-skipped-tests *report-skipped-tests*)
  (sb-ext:exit :code (if (unexpected-failures)
                         1
                         104)))

(defun pure-runner (files test-fun)
  (when files
    (format t "// Running pure tests (~a)~%" test-fun)
    (let ((*package* (find-package :cl-user))
          (test-util::*results* '()))
      (setup-cl-user)
      (dolist (file files)
        (format t "// Running ~a in ~a evaluator mode~%"
                file *test-evaluator-mode*)
        (restart-case
            (handler-bind ((error (make-error-handler file)))
              (let* ((sb-ext:*evaluator-mode* *test-evaluator-mode*)
                     (*features*
                       (if (eq sb-ext:*evaluator-mode* :interpret)
                           (cons :interpreter *features*)
                           *features*)))
                (funcall test-fun file)))
          (skip-file ())))
      (append-results))))

(defun run-in-child-sbcl (&rest evals)
  (process-exit-code
   (sb-ext:run-program
    (first *POSIX-ARGV*)
    (list* "--core" SB-INT:*CORE-STRING*
           "--noinform"
           "--no-sysinit"
           "--no-userinit"
           "--noprint"
           "--disable-debugger"
           (loop for eval in evals
              collect "--eval"
              collect (write-to-string eval :right-margin 1000)))
    :output t
    :input t)))

(defun run-impure-in-child-sbcl (test-file test-fun)
  (clear-test-status)
  (run-in-child-sbcl
   '(cl:require "asdf")
   '(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration))
   '(cl:require "sb-test")

   '(cl:in-package "CL-USER") ; TODO (setup-cl-user)
   '(cl:use-package "TEST-UTIL")

   `(run-tests::run
     ,test-file
     ',test-fun
     ,*break-on-failure*
     ,*break-on-expected-failure*
     ,*break-on-error*
     ,(eq *test-evaluator-mode* :interpret))))

(defun impure-runner (files test-fun)
  (when files
    (format t "// Running impure tests (~a)~%" test-fun)
    (dolist (file files)
      (force-output)
      (let ((exit-code (run-impure-in-child-sbcl file test-fun)))
        (if (= exit-code 104)
            (with-open-file (stream "test-status.lisp-expr"
                                    :direction :input
                                    :if-does-not-exist :error)
              (append-results (read stream)))
            (push (test-util::invalid-exit-status file 'TODO 0 exit-code)
                  *all-results*))))))

(defun make-error-handler (file)
  (lambda (condition)
    (push (test-util::unhandled-error file 'dummy 0 condition)
          test-util::*results*)
    (cond (*break-on-error*
           (test-util:really-invoke-debugger condition))
          (t
           (format *error-output* "~&Unhandled ~a: ~a~%"
                   (type-of condition) condition)
           (sb-debug:print-backtrace)))
    (invoke-restart 'skip-file)))

(defun append-results (&optional (results test-util::*results*))
  (setf *all-results* (append results *all-results*)))

(defun unexpected-failures ()
  (remove-if (lambda (result)
               (typep result '(or test-util::expected-failure
                                  test-util::unexpected-success
                                  test-util::skipped-broken
                                  test-util::skipped-disabled
                                  test-util::success)))
             *all-results*))

(defun setup-cl-user ()
  (use-package "TEST-UTIL"))

(defun filter-test-files (wild-mask)
  (if *explicit-test-files*
      (loop for file in *explicit-test-files*
            when (pathname-match-p file wild-mask)
            collect file)
      (directory wild-mask)))

(defun pure-load-files ()
  (filter-test-files "*.pure.lisp"))

(defun pure-cload-files ()
  (filter-test-files "*.pure-cload.lisp"))

(defun impure-load-files ()
  (filter-test-files "*.impure.lisp"))

(defun impure-cload-files ()
  (filter-test-files "*.impure-cload.lisp"))

(defun sh-files ()
  (filter-test-files "*.test.sh"))

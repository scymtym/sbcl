#+#.(cl:if (cl:find-package "ASDF") '(or) '(and))
(require :asdf)

#+#.(cl:if (cl:find-package "SB-POSIX") '(or) '(and))
(handler-bind (#+win32 (warning #'muffle-warning))
  (require :sb-posix))

(load "test-util.lisp")

(defpackage :run-tests
    (:use :cl :test-util :sb-ext))

(load "assertoid.lisp")

(in-package run-tests)

(load "colorize.lisp")

(defvar *all-results* nil)
(defvar *break-on-error* nil)
(defvar *accept-files* nil)
(defvar *report-skipped-tests* nil)
(defvar *report-style* :describe)
(defvar *report-target* *standard-output*)

(defun run-all ()
  (let ((args (rest *posix-argv*)))
    (do ((arg (pop args) (pop args)))
        ((null arg))
      (cond ((string= arg "--break-on-failure")
             (setf *break-on-error* t)
             (setf test-util:*break-on-failure* t))
            ((string= arg "--break-on-expected-failure")
             (setf test-util:*break-on-expected-failure* t))
            ((string= arg "--report-skipped-tests")
             (setf *report-skipped-tests* t))
            ((string= arg "--report-style")
             (let* ((name (or (pop args)
                              (error "~@<Missing argument for ~A option.~@:>"
                                     arg)))
                    (style (intern (string-upcase name) :keyword)))
               (setf *report-style* style)))
            ((string= arg "--report-target")
             (setf *report-target*
                   (or (pop args)
                       (error "~@<Missing argument for ~A option.~@:>"
                              arg))))
            ((string= arg "--report-skipped-tests")
             (setf *report-skipped-tests* t))
            ((string= arg "--no-color"))
            (t
             (push (truename (parse-namestring arg)) *accept-files*)))))
  (pure-runner (pure-load-files) #'load-test)
  (pure-runner (pure-cload-files) #'cload-test)
  (impure-runner (impure-load-files) #'load-test)
  (impure-runner (impure-cload-files) #'cload-test)
  #-win32 (impure-runner (sh-files) #'sh-test)
  (report *all-results*)
  (sb-ext:exit :code (if (unexpected-failures *all-results*)
                         1
                         104)))

(defun pure-runner (files test-fun)
  (format t "// Running pure tests (~a)~%" test-fun)
  (let ((*package* (find-package :cl-user))
        (*results* '()))
    (setup-cl-user)
    (dolist (file files)
      (when (accept-test-file file)
        (format t "// Running ~a~%" file)
        (restart-case
            (handler-bind ((error (make-error-handler file)))
              (eval (funcall test-fun file)))
          (skip-file ()))))
    (append-results)))

(defun run-in-child-sbcl (load-forms forms)
  ;; We used to fork() for POSIX platforms, and use this for Windows.
  ;; However, it seems better to use the same solution everywhere.
  (process-exit-code
   (#-win32 with-open-file #-win32 (devnull "/dev/null") #+win32 progn
     (sb-ext:run-program
      (first *POSIX-ARGV*)
      (append
       (list "--core" SB-INT:*CORE-STRING*
             "--noinform"
             "--no-sysinit"
             "--no-userinit"
             "--noprint"
             "--disable-debugger")
       (loop for form in (append load-forms forms)
             collect "--eval"
             collect (write-to-string form)))
      :output sb-sys:*stdout*
      :input #-win32 devnull #+win32 sb-sys:*stdin*))))

(defun run-impure-in-child-sbcl (test-file test-code)
  (run-in-child-sbcl
    `((load "test-util")
      (load "assertoid")
      (defpackage :run-tests
        (:use :cl :test-util :sb-ext)))

    `((in-package :cl-user)
      (use-package :test-util)
      (use-package :assertoid)
      (setf test-util:*break-on-failure* ,test-util:*break-on-failure*)
      (setf test-util:*break-on-expected-failure*
            ,test-util:*break-on-expected-failure*)
      (let ((file ,test-file)
            (*break-on-error* ,run-tests::*break-on-error*))
        (declare (special *break-on-error*))
        (format t "// Running ~a~%" file)
        (restart-case
            (handler-bind
                ((error (lambda (condition)
                          (push (make-result :file file :status :unhandled-error)
                                test-util:*results*)
                          (cond (*break-on-error*
                                 (test-util:really-invoke-debugger condition))
                                (t
                                 (format *error-output* "~&Unhandled ~a: ~a~%"
                                         (type-of condition) condition)
                                 (sb-debug:print-backtrace)))
                          (invoke-restart 'skip-file))))
              ,test-code)
          (skip-file ()
            (format t ">>>~a<<<~%" test-util:*results*)))
        (test-util:report-test-status)
        (sb-ext:exit :code 104)))))

(defun impure-runner (files test-fun)
  (format t "// Running impure tests (~a)~%" test-fun)
  (let ((*package* (find-package :cl-user)))
    (setup-cl-user)
    (dolist (file files)
      (when (accept-test-file file)
        (force-output)
        (let ((exit-code (run-impure-in-child-sbcl file
                                                   (funcall test-fun file))))
          (if (= exit-code 104)
              (with-open-file (stream "test-status.lisp-expr"
                                      :direction :input
                                      :if-does-not-exist :error)
                (append-results
                 (sb-ext:without-package-locks ; test names may contain such symbols
                   (read stream))))
              (push (make-result :file file :status :invalid-exit-status)
                    *all-results*)))))))

(defun make-error-handler (file)
  (lambda (condition)
    (push (make-result :file file :status :unhandled-error) *results*)
    (cond (*break-on-error*
           (test-util:really-invoke-debugger condition))
          (t
           (format *error-output* "~&Unhandled ~a: ~a~%"
                   (type-of condition) condition)
           (sb-debug:print-backtrace)))
    (invoke-restart 'skip-file)))

(defun setup-cl-user ()
  (use-package :test-util)
  (use-package :assertoid))

(defun load-test (file)
  `(load ,file))

(defun cload-test (file)
  `(let ((compile-name (compile-file-pathname ,file)))
     (unwind-protect
          (progn
            (compile-file ,file)
            (load compile-name))
       (ignore-errors
         (delete-file compile-name)))))

(defun sh-test (file)
  ;; What? No SB-POSIX:EXECV?
  `(let ((process (sb-ext:run-program "/bin/sh"
                                      (list (native-namestring ,file))
                                      :output *error-output*)))
     (let ((*results* '()))
       (test-util:report-test-status))
     (sb-ext:exit :code (process-exit-code process))))

(defun accept-test-file (file)
  (if *accept-files*
      (find (truename file) *accept-files* :test #'equalp)
      t))

(defun pure-load-files ()
  (directory "*.pure.lisp"))

(defun pure-cload-files ()
  (directory "*.pure-cload.lisp"))

(defun impure-load-files ()
  (directory "*.impure.lisp"))

(defun impure-cload-files ()
  (directory "*.impure-cload.lisp"))

(defun sh-files ()
  (directory "*.test.sh"))

;;; Result handling

(defun append-results (&optional (results *results*))
  (setf *all-results* (append results *all-results*)))

(defun failures (results)
  (remove :success results :key #'result-status))

(defun unexpected-failures (results)
  (remove-if (lambda (x)
               (member (result-status x)
                       '(:success
                         :expected-failure
                         :unexpected-success
                         :skipped-broken
                         :skipped-disabled)))
             results))

;;; Result reporting

(defun report (results &key (style *report-style*) (target *report-target*))
  (report-using-style results style target))

(defgeneric report-using-style (results style target))

(defmethod report-using-style ((results t) (style t) (target string))
  (report-using-style results style (parse-namestring target)))

(defmethod report-using-style ((results t) (style t) (target pathname))
  (with-open-file (stream target :if-does-not-exist :create
                                 :direction :output
                                 :if-exists :supersede)
    (report-using-style results style stream)))

(defmethod report-using-style ((results t) (style (eql :describe)) (target stream))
  (terpri target)
  (format target "Finished running tests.~%")
  (let ((skipcount 0)
        (*print-pretty* nil))
    (cond ((failures results)
           (format target "Status:~%")
           (dolist (failure (reverse (failures results)))
             (with-accessors ((status result-status)
                              (file result-file)
                              (condition result-condition)) failure
               (case status
                 (:unhandled-error
                  (output-colored-text status
                                       " Unhandled Error")
                  (format target " ~a~%"
                          (enough-namestring file)))
                 (:invalid-exit-status
                  (output-colored-text status
                                       " Invalid exit status:")
                  (format target " ~a~%"
                          (enough-namestring file)))
                 (:skipped-disabled
                  (when *report-skipped-tests*
                    (format target " ~20a ~a / ~a~%"
                            "Skipped (irrelevant):"
                            (enough-namestring file)
                            condition))
                  (incf skipcount))
                 (t
                  (output-colored-text status
                                       (ecase status
                                         (:expected-failure " Expected failure:")
                                         (:unexpected-failure " Failure:")
                                         (:leftover-thread " Leftover thread (broken):")
                                         (:unexpected-success " Unexpected success:")
                                         (:skipped-broken " Skipped (broken):")
                                         (:skipped-disabled " Skipped (irrelevant):")))
                  (format target " ~a / ~a~%"
                          (enough-namestring file)
                          condition)))))
           (when (> skipcount 0)
             (format target " (~a tests skipped for this combination of platform and features)~%"
                     skipcount)))
          (t
           (format target "All tests succeeded~%")))))

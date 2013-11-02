(defpackage :benchmark-util
  (:use :cl :sb-ext)
  (:export #:result-file #:result-name #:result-status #:result-condition #:make-result
           #:result-failure-p #:result-error-p
           #:with-test #:report-test-status #:*results*
           #:really-invoke-debugger
           #:*break-on-failure* #:*break-on-expected-failure*
           #:make-kill-thread #:make-join-thread
           #:runtime))

(in-package :benchmark-util)

(defvar *test-count* 0)
(defvar *test-file* nil)
(defvar *results* '())
(defvar *break-on-failure* nil)
(defvar *break-on-expected-failure* nil)

(defvar *threads-to-kill*)
(defvar *threads-to-join*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

;;; run-program on Windows doesn't have an :environment parameter,
;;; set these globally
(sb-posix:putenv (format nil "SBCL_MACHINE_TYPE=~A" (machine-type)))
(sb-posix:putenv (format nil "SBCL_SOFTWARE_TYPE=~A" (software-type)))

#+sb-thread
(defun make-kill-thread (&rest args)
  (let ((thread (apply #'sb-thread:make-thread args)))
    (when (boundp '*threads-to-kill*)
      (push thread *threads-to-kill*))
    thread))

#+sb-thread
(defun make-join-thread (&rest args)
  (let ((thread (apply #'sb-thread:make-thread args)))
    (when (boundp '*threads-to-join*)
      (push thread *threads-to-join*))
    thread))

(defun log-msg (&rest args)
  (format *trace-output* "~&::: ")
  (apply #'format *trace-output* args)
  (terpri *trace-output*)
  (force-output *trace-output*))

(defun required-argument (&optional name)
  (error "Missing required argument~@[ ~S~]" name))

(defstruct result
  (file (required-argument :file) :type pathname :read-only t)
  (name nil :type (or null string symbol cons) :read-only t)
  (status (required-argument :status) :type keyword :read-only t)
  (condition nil :type (or null string condition) :read-only t))

(defun result-failure-p (result)
  (member (result-status result)
          '(:unexpected-failure :leftover-thread
            :unexpected-success)))

(defun result-error-p (result)
  (member (result-status result)
          '(:unhandled-error :invalid-exit-status)))

(defmethod print-object ((object result) stream)
  (if *print-escape*
      (call-next-method)
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~A ~A" (result-name object) (result-status object)))))

(defmacro with-benchmark ((&key skipped-on name) &body body)
  (let ((block-name (gensym)))
    `(progn
       (start-benchmark)
       (cond
         ((skipped-p ,skipped-on)
          (fail-test :skipped-disabled ',name "Benchmark disabled for this combination of platform and features"))
         (t
          (block ,block-name
            (handler-bind ((error (lambda (error)
                                    (if (expected-failure-p ,fails-on)
                                        (fail-test :expected-failure ',name error)
                                        (fail-test :unexpected-failure ',name error))
                                    (return-from ,block-name))))
              (log-msg "Running ~S" ',name)
              ,@body                
              (push (make-result :file *test-file*
                                 :name (or ',name *test-count*)
                                 :status :success)
                    *results*)
              (log-msg "Success ~S" ',name))))))))

(defun report-test-status ()
  (with-standard-io-syntax
    (with-open-file (stream "test-status.lisp-expr"
                            :direction :output
                            :if-exists :supersede)
      (format stream "~s~%" *results*))))

(defun start-test ()
  (unless (eq *test-file* *load-pathname*)
    (setf *test-file* *load-pathname*)
    (setf *test-count* 0))
  (incf *test-count*))

(defun really-invoke-debugger (condition)
  (with-simple-restart (continue "Continue")
    (let ((*invoke-debugger-hook* *invoke-debugger-hook*))
      (enable-debugger)
      (invoke-debugger condition))))

(defun fail-test (type test-name condition)
  (if (stringp condition)
      (log-msg "~@<~A ~S ~:_~A~:>"
               type test-name condition)
      (log-msg "~@<~A ~S ~:_due to ~S: ~4I~:_\"~A\"~:>"
               type test-name condition condition))
  (push (make-result :file *test-file*
                     :name (or test-name *test-count*)
                     :status type
                     :condition (princ-to-string condition))
        *results*)
  (unless (stringp condition)
    (when (or (and *break-on-failure*
                   (not (eq type :expected-failure)))
              *break-on-expected-failure*)
      (really-invoke-debugger condition))))

(defun expected-failure-p (fails-on)
  (sb-impl::featurep fails-on))

(defun broken-p (broken-on)
  (sb-impl::featurep broken-on))

(defun skipped-p (skipped-on)
  (sb-impl::featurep skipped-on))

;;; Repeat calling THUNK until its cumulated runtime, measured using
;;; GET-INTERNAL-RUN-TIME, is larger than PRECISION. Repeat this
;;; REPETITIONS many times and return the time one call to THUNK took
;;; in seconds as a float, according to the minimum of the cumulated
;;; runtimes over the repetitions.
;;; This allows to easily measure the runtime of expressions that take
;;; much less time than one internal time unit. Also, the results are
;;; unaffected, modulo quantization effects, by changes to
;;; INTERNAL-TIME-UNITS-PER-SECOND.
;;; Taking the minimum is intended to reduce the error introduced by
;;; garbage collections occurring at unpredictable times. The inner
;;; loop doubles the number of calls to THUNK each time before again
;;; measuring the time spent, so that the time measurement overhead
;;; doesn't distort the result if calling THUNK takes very little time.
(defun runtime* (thunk repetitions precision)
  (loop repeat repetitions
        minimize
        (loop with start = (get-internal-run-time)
              with duration = 0
              for n = 1 then (* n 2)
              for total-runs = n then (+ total-runs n)
              do (dotimes (i n)
                   (funcall thunk))
                 (setf duration (- (get-internal-run-time) start))
              when (> duration precision)
              return (/ (float duration) (float total-runs)))
        into min-internal-time-units-per-call
        finally (return (/ min-internal-time-units-per-call
                           (float internal-time-units-per-second)))))

(defmacro runtime (form &key (repetitions 3) (precision 10))
  `(runtime* (lambda () ,form) ,repetitions ,precision))

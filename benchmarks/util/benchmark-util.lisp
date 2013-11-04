(defpackage :benchmark-util
  (:use #:cl #:sb-ext)
  (:export #:with-benchmark
           #:measuring

   #:result-file #:result-name #:result-status #:result-condition #:make-result
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))


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
  (declare (type function thunk))
  (let ((min-internal-time-units-per-call nil)
        (min-bytes-consed-per-call nil))
    (loop repeat repetitions
          do (loop with start = (get-internal-run-time)
                   with start-consed = (nth-value 3 (sb-impl::time-get-sys-info))
                   with duration = 0
                   with bytes-consed = 0
                   for n = 1 then (* n 2)
                   for total-runs = n then (+ total-runs n)
                   do (dotimes (i n) (funcall thunk))
                      (setf duration (- (get-internal-run-time) start)
                            bytes-consed (- (nth-value 3 (sb-impl::time-get-sys-info))
                                            start-consed))
                   until (> duration precision)
                   finally (let ((internal-time-units-per-call
                                   (/ (float duration) (float total-runs)))
                                 (bytes-consed-per-call
                                   (/ (float bytes-consed) (float total-runs))))
                             (when (or (not min-internal-time-units-per-call)
                                       (< internal-time-units-per-call
                                          min-internal-time-units-per-call))
                               (setf min-internal-time-units-per-call internal-time-units-per-call
                                     min-bytes-consed-per-call bytes-consed-per-call)))))
    (values (/ min-internal-time-units-per-call
               (float internal-time-units-per-second))
            (ceiling min-bytes-consed-per-call))))

(defmacro runtime (form &key (repetitions 3) (precision 10))
  `(runtime* (lambda () ,form) ,repetitions ,precision))

(defun call-measuring (thunk)
  "TODO(jmoringe): document"
  (runtime* thunk 3 10))

(defmacro with-measuring (() &body body)
  "TODO(jmoringe): document"
  `(macrolet ((measuring (&body body)
                `(call-measuring (lambda () ,@body))))
     ,@body))

(defun call-with-parameters (parameter-values thunk)
  (declare (function thunk))
  (let ((args (make-list (length parameter-values))))
    (declare (dynamic-extent args))
    (labels ((bind-parameters (head values)
               (destructuring-bind
                   (&optional first-values &rest rest-values) values
                (cond
                  (rest-values
                   (dolist (value first-values)
                     (setf (car head) value)
                     (bind-parameters (rest head) rest-values)))
                  (first-values
                   (dolist (value first-values)
                     (setf (car head) value)
                     (apply thunk args)))
                  (t (funcall thunk))))))
      (bind-parameters args parameter-values))))

(defun parse-parameter-spec (spec)
  (labels ((parse-value-spec (spec)
             (etypecase spec
               ((cons (eql :expt) (cons integer))
                (mapcar (lambda (value)
                          (expt (second spec) value))
                        (parse-value-spec (nth 2 spec))))
               ((cons (eql :iota))
                (destructuring-bind (size &optional (start 0) (step 1))
                    (rest spec)
                  (loop :repeat size :for i :from start :by step
                        :collect i)))
               (cons
                spec))))
   (destructuring-bind (name values) spec
     (list name (parse-value-spec values)))))

(defun parameter-name (parameter)
  (first parameter))

(defun parameter-values (parameter)
  `(list ,@(second parameter)))

(defmacro with-parameters ((&rest parameter-specs)
                           &body body)
  (let ((parameters
          (mapcar #'parse-parameter-spec parameter-specs)))
    `(call-with-parameters
      (list ,@(mapcar #'parameter-values parameters))
      (lambda ,(mapcar #'parameter-name parameters) ,@body))))

(with-parameters ((foo (1 2 3 4)) (bar (2 3 4)))
  (print (list foo bar)))

(defmacro with-benchmark ((&key
                           name
                           skipped-on
                           parameters)
                          &body body)
  (let ((parameters/parsed (mapcar #'parse-parameter-spec parameters))
        (block-name (gensym))
        (parameters-var (gensym))
        (runtime-var (gensym))
        (bytes-consed-var (gensym)))
    `(progn
       #+later (start-benchmark)
       (cond
         #+later ((skipped-p ,skipped-on)
          (fail-test :skipped-disabled ',name "Benchmark disabled for this combination of platform and features"))
         (t
          (with-parameters ,parameters
            (let ((,parameters-var (list ,@(reduce #'append parameters/parsed
                                                   :key (lambda (parameter)
                                                          (list `(quote ,(parameter-name parameter))
                                                                (parameter-name parameter)))))))
              (block ,block-name
                (handler-bind ((error (lambda (error)
                                        (fail-benchmark :expected-failure ',name error)
                                        (return-from ,block-name))))
                  (log-msg "Running ~S ~@[@ ~S~]" ',name ,parameters-var)
                  (multiple-value-bind (,runtime-var ,bytes-consed-var)
                      (with-measuring () ,@body)
                    #+later (push (make-result :file *test-file*
                                               :name (or ',name *test-count*)
                                               :status :success)
                                  *results*)
                    (log-msg "Success ~S ~@[@ ~S~]~%:::   ~A s; ~D byte~:P"
                             ',name ,parameters-var ,runtime-var ,bytes-consed-var)))))))))))

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

(defun fail-benchmark (type test-name condition)
  (if (stringp condition)
      (log-msg "~@<~A ~S ~:_~A~:>"
               type test-name condition)
      (log-msg "~@<~A ~S ~:_due to ~S: ~4I~:_\"~A\"~:>"
               type test-name condition condition))
  #+later (push (make-result :file *test-file*
                     :name (or test-name *test-count*)
                     :status type
                     :condition (princ-to-string condition))
        *results*)
  #+later
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

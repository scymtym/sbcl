(cl:use-package :benchmark-util)

;;; Benchmark establishing of restarts

(with-benchmark (:name (restart-bind)
                 :parameters ((num-clusters (:expt 10 (:iota 5)))))
  (labels ((clusters (remaining)
             (unless (zerop remaining)
               (restart-bind ((continue (lambda (&optional condition)
                                          (declare (ignore condition))))
                              (retry (lambda ()))
                              (use-value (lambda (value)
                                           (declare (ignore value)))))
                 (clusters (1- remaining))))))
    (measuring (clusters num-clusters))))

(with-benchmark (:name (restart-case)
                 :parameters ((num-clusters (:expt 10 (:iota 5)))))
  (labels ((clusters (remaining)
             (unless (zerop remaining)
               (restart-case (clusters (1- remaining))
                 (continue (&optional condition)
                   (declare (ignore condition)))
                 (retry ())
                 (use-value (value)
                   (declare (ignore value)))))))
    (measuring (clusters num-clusters))))

;;; Benchmark finding established restarts

;; Call THUNK with TODO
;;
;; Note: this is only suitable for benchmarking the behavior of THUNK,
;; not for benchmarking establishing of restarts.
(defun call-with-established-restarts (num-clusters condition? thunk)
  (let ((condition (when condition?
                     (make-instance 'simple-error :format-control "foo"))))
    (labels ((call-thunk ()
               (if condition
                   (funcall thunk condition)
                   (funcall thunk)))
             (clusters (remaining)
               (flet ((next ()
                        (if (zerop remaining)
                            (call-thunk)
                            (clusters (1- remaining)))))
                 (restart-case
                     (if condition
                         (with-condition-restarts condition
                             (mapcar #'find-restart '(continue retry use-value))
                           (next))
                         (next))
                   (continue (&optional condition)
                     (declare (ignore condition)))
                   (retry ())
                   (use-value (value)
                     (declare (ignore value)))))))
      (if (zerop num-clusters)
          (call-thunk)
          (restart-case
              (clusters num-clusters)
            (outermost ()))))))

(defmacro with-established-restarts ((&key
                                      (num-clusters 1000)
                                      (condition    nil))
                                     &body body)
  `(call-with-established-restarts
    ,num-clusters ,(when condition t)
    (lambda (,@(when condition `(,condition)))
      ,@(when condition `((declare (ignorable ,condition))))
      ,@body)))

(with-benchmark (:name (find-restart :symbol :without-condition)
                 :parameters ((restart      ('continue 'outermost))
                              (num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters)
    (case restart
      (continue  (measuring (find-restart 'continue)))
      (outermost (measuring (find-restart 'outermost))))))
(defclass)
(with-benchmark (:name (compute-restarts :symbol :with-condition)
                 :parameters ((restart      ('continue 'outermost))
                              (num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters
                              :condition condition)
    (case restart
      (continue  (measuring (find-restart 'continue condition)))
      (outermost (measuring (find-restart 'outermost condition))))))

(with-benchmark (:name (compute-restarts :without-condition)
                 :parameters ((num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters)
    (measuring (compute-restarts))))

(with-benchmark (:name (compute-restarts :with-condition)
                 :parameters ((num-clusters (:expt 10 (:iota 4)))))
  (with-established-restarts (:num-clusters num-clusters
                              :condition-var condition)
    (measuring (compute-restarts condition))))

(defun get-time ()
  #+no (multiple-value-bind (sec nsec) (sb-ext:get-time-of-day)
         (+ sec (/ nsec 1000000000d0)))
  (/ (get-internal-real-time) (float internal-time-units-per-second 1.0d0)))

(defun find-some-restarts (depth &rest args
                                 &key
                                 symbol?
                                 symbol-condition?
                                 restart?
                                 restart-condition?
                                 compute-restarts?
                                 compute-restarts-condition?)
  (let ((condition (make-instance 'simple-error :format-control "foo")))
    (restart-case
        (with-condition-restarts condition
            (mapcar #'find-restart '(continue retry use-value))
          (if (zerop depth)
              (time
               (let ((start (get-time)))
                 (let ((r1 (find-restart 'continue))
                       (r2 (find-restart 'retry))
                       (r3 (find-restart 'use-value)))
                   (loop
                     repeat 10000
                     do
                        (when symbol?
                          (find-restart 'continue)
                          (find-restart 'retry)
                          (find-restart 'use-value))
                        (when symbol-condition?
                          (find-restart 'continue condition)
                          (find-restart 'retry condition)
                          (find-restart 'use-value condition))
                        (when restart?
                          (find-restart r1)
                          (find-restart r2)
                          (find-restart r3))
                        (when restart-condition?
                          (find-restart r1 condition)
                          (find-restart r2 condition)
                          (find-restart r3 condition))
                        (when compute-restarts?
                          (compute-restarts))
                        (when compute-restarts-condition?
                          (compute-restarts condition)))
                   (- (get-time) start))))
              (apply #'find-some-restarts (1- depth) args)))
      (continue (&optional condition)
        (declare (ignore condition)))
      (retry ())
      (use-value (value)
        (declare (ignore value))))))

;; Warmup

(dolist (which '(:symbol? :symbol-condition? :restart? :restart-condition?
                 :compute-restarts? :compute-restarts-condition?))
  (find-some-restarts 20 which t))

#+no (with-open-file (stream (format nil "times-~A.txt" (lisp-implementation-version))
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create)
       (dolist (which '(:symbol? :symbol-condition? :restart? :restart-condition?
                        :compute-restarts? :compute-restarts-condition?))
         (format t "~36A~%" which)
         (dolist (i '(0 1 2 3))
           (let ((depth (expt 10 i)))
             (format t "~2@TDepth ~D~%" depth)
             (format stream "~36F " (find-some-restarts depth which t)))))
       (terpri stream))

(sb-int:collect ((result))
  (dolist (which '(:symbol? :symbol-condition? :restart? :restart-condition?
                   :compute-restarts? :compute-restarts-condition?))
    (format t "~36A~%" which)
    (dolist (i '(0 1 2 3))
      (let ((depth (expt 10 i)))
        (format t "~2@TDepth ~D~%" depth)
        (result (list (intern (format nil "~A-~D" which depth) :keyword)
                      (find-some-restarts depth which t))))))
  (with-open-file (stream (format nil "times-~A.txt" (lisp-implementation-version))
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)

    (print (result) stream)))

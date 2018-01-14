(cl:in-package "TEST-UTIL")

(defun report (results &rest keys &key
                                    (style :describe)
                                    (target *standard-output*)
                                    &allow-other-keys)
  (let ((style (list* (case style
                        (:describe 'style-describe)
                        (t style))
                      (loop for (key value) on keys by #'cddr
                            unless (member key '(:style :target))
                            collect key collect value))))
   (report-using-style results style target)))

(defgeneric report-using-style (results style target))

(defmethod report-using-style ((results t) (style cons) (target t))
  (let ((style (apply #'make-instance style)))
    (report-using-style results style target)))

(defmethod report-using-style ((results t) (style t) (target string))
  (report-using-style results style (parse-namestring target)))

(defmethod report-using-style ((results t) (style t) (target pathname))
  (with-open-file (stream target :if-does-not-exist :create
                          :direction :output
                          :if-exists :supersede)
    (report-using-style results style stream)))

;;;; "describe" report style

(defclass style-describe ()
  ((report-skipped-tests :initarg  :report-skipped-tests
                         :reader   report-skipped-tests
                         :initform t)))

(defun failure-info (failure)
  (ecase (car failure)
    (:unhandled-error
     (values "Unhandled Error" '(:red :bold t)))
    (:invalid-exit-status
     (values "Invalid exit status" '(:red :bold t)))
    (:unexpected-failure
     (values "Failure:" '(:red :bold t)))
    (:leftover-thread
     (values "Leftover thread (broken):" '(:red :bold t)))
    (:unexpected-success
     (values "Unexpected success:" '(:green)))
    (:expected-failure
     "Expected failure:")
    (:skipped-broken
     "Skipped (broken):")
    (:skipped-disabled
     "Skipped (irrelevant):")))

(defmethod report-using-style
    ((results t) (style style-describe) (target stream))
  (terpri target)
  (format target "Finished running tests.~%")
  (let ((all-failures results)
        (skip-count 0)
        (*print-pretty* nil))
    (cond (all-failures
           (format target "Status:~%")
           (dolist (fail (reverse all-failures))
             (let ((kind (car fail)))
               (multiple-value-bind (label color) (failure-info fail)
                 (labels ((label (stream)
                            (format stream "~26@< ~A~>" label))
                          (output ()
                            (if color
                                (apply #'call-with-colored-output
                                       #'label target color)
                                (label target))
                            (format target " ~A~@[ / ~A~]~%"
                                    (enough-namestring (second fail))
                                    (third fail))))
                   (case kind
                     (:skipped-disabled
                      (when (report-skipped-tests style)
                        (output))
                      (incf skip-count))
                     (t
                      (output)))))))
           (when (> skip-count 0)
             (format target " (~a tests skipped for this combination ~
                             of platform and features)~%"
                     skip-count)))
          (t
           (format target "All tests succeeded~%")))))

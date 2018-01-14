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

(defmethod report-using-style
    ((results t) (style style-describe) (target stream))
  (terpri target)
  (format target "Finished running tests.~%")
  (let ((all-failures results)
        (skipcount 0)
        (*print-pretty* nil))
    (cond (all-failures
           (format target "Status:~%")
           (dolist (fail (reverse all-failures))
             (cond ((eq (car fail) :unhandled-error)
                    (output-colored-text (car fail)
                                         " Unhandled Error")
                    (format target " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :invalid-exit-status)
                    (output-colored-text (car fail)
                                         " Invalid exit status:")
                    (format target " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :skipped-disabled)
                    (when (report-skipped-tests style)
                      (format target " ~20a ~a / ~a~%"
                              "Skipped (irrelevant):"
                              (enough-namestring (second fail))
                              (third fail)))
                    (incf skipcount))
                   (t
                    (output-colored-text
                     (first fail)
                     (ecase (first fail)
                       (:expected-failure " Expected failure:")
                       (:unexpected-failure " Failure:")
                       (:leftover-thread " Leftover thread (broken):")
                       (:unexpected-success " Unexpected success:")
                       (:skipped-broken " Skipped (broken):")
                       (:skipped-disabled " Skipped (irrelevant):")))
                    (format target " ~a / ~a~%"
                            (enough-namestring (second fail))
                            (third fail)))))
           (when (> skipcount 0)
             (format target " (~a tests skipped for this combination of platform and features)~%"
                     skipcount)))
          (t
           (format target "All tests succeeded~%")))))

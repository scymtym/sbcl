(cl:in-package "TEST-UTIL")

(defun report (all-failures &key report-skipped-tests (stream *standard-output*))
  (terpri stream)
  (format stream "Finished running tests.~%")
  (let ((skipcount 0)
        (*print-pretty* nil))
    (cond (all-failures
           (format stream "Status:~%")
           (dolist (fail (reverse all-failures))
             (cond ((eq (car fail) :unhandled-error)
                    (output-colored-text (car fail)
                                         " Unhandled Error")
                    (format stream " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :invalid-exit-status)
                    (output-colored-text (car fail)
                                         " Invalid exit status:")
                    (format stream " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :skipped-disabled)
                    (when report-skipped-tests
                      (format stream " ~20a ~a / ~a~%"
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
                    (format stream " ~a / ~a~%"
                            (enough-namestring (second fail))
                            (third fail)))))
           (when (> skipcount 0)
             (format stream " (~a tests skipped for this combination of platform and features)~%"
                     skipcount)))
          (t
           (format stream "All tests succeeded~%")))))

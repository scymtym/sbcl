(cl:in-package "TEST-UTIL")

(defun ascii-color (color)
  (ecase color
    (:black   30)
    (:red     31)
    (:green   32)
    (:yellow  33)
    (:blue    34)
    (:magenta 35)
    (:cyan    36)
    (:white   37)))

(defun stream-underlying-stream (stream)
  (sb-impl::stream-output-stream stream))

(defun call-without-changing-column (thunk stream)
  (let ((old-column (when (typep stream 'sb-sys:fd-stream)
                      (sb-impl::fd-stream-output-column stream))))
    (funcall thunk stream)
    (when old-column
      (setf (sb-impl::fd-stream-output-column stream) old-column))))

(defun %call-with-colored-output (thunk stream color bold)
  (let ((output-stream (sb-impl::stream-output-stream stream)))
    (finish-output stream)
    (call-without-changing-column
     (lambda (stream)
       (format stream "~C[~A~:[~;;1~]m"
               #\Esc (ascii-color color) bold))
     output-stream)
    (unwind-protect
         (funcall thunk stream)
      (call-without-changing-column
       (lambda (stream)
         (format stream "~C[0m" #\Esc))
       output-stream))))

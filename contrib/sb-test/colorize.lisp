(cl:in-package "TEST-UTIL")

(defvar *no-color*
  (member "--no-color" *posix-argv* :test #'equal))

(defvar *color-error* nil)

(defun is-tty (stream)
  (let* ((stream (sb-impl::stream-output-stream *standard-output*))
         (fd (and (sb-sys:fd-stream-p stream)
                  (sb-sys:fd-stream-fd stream))))
    (when (integerp fd)
      (plusp (sb-unix:unix-isatty fd)))))

(defun present-coloring-error (error)
  (format t "~a~%" error)
  (format t "Switching off colored output,~%~
             it can be turned off by passing --no-color~%~%")
  (setf *no-color* t))

(defun call-with-colored-output (thunk stream color &key bold)
  (cond ((or (not (is-tty stream))
             *no-color*)
         (funcall thunk stream))
        (*color-error*
         (present-coloring-error *color-error*)
         (funcall thunk stream))
        (t
         (handler-case
             (%call-with-colored-output thunk stream color bold)
           (error (c)
             (present-coloring-error
              (format nil "Error while printing colored text:~% ~a"
                      c))
             (funcall thunk stream))))))

(defmacro with-colored-output ((stream-variable color &key bold) &body body)
  (check-type stream-variable symbol)
  `(call-with-colored-output
    (lambda (,stream-variable) ,@body)
    ,stream-variable ,color :bold ,bold))

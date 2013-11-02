(let ((implementations '())
      (cases           '()))
  (dolist (file (directory "times-*.txt"))
    (with-open-file (stream file)
      (push (pathname-name file) implementations)
      (let ((data (read stream)))
        (dolist (case data)
          (let ((cell (or (assoc (first case) cases)
                          (let ((cell (list (first case))))
                            (push cell cases)
                            cell))))
            (push (second case) (cdr cell)))))))

  (with-open-file (stream "data.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (case (sort cases #'string< :key #'first))
      (destructuring-bind (which first &rest rest) case
        (format stream "~36A~{ ~36F~}~%"
                which (cons 1 (mapcar (lambda (x) (/ x first)) rest)))))
    (terpri stream))

  (with-open-file (stream "plot.plt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "
set terminal png font \",8\" size 1400,800
set output \"~A.png\"

set style data histogram

set log y
set yrange [0.0001:10000]

set grid
set style fill solid border -1
set boxwidth 0.9
set xtic rotate by -45

plot ~{\"data.txt\" ~{using ~D:xtic(1) title ~S~}~^, ~}"
            (lisp-implementation-version)
            (loop for i from 2
                  for name in implementations
                  collect (list i name))))


  (run-program "gnuplot" '("gnuplot" "plot.plt") :search t))

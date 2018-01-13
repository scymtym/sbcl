;;; -*-  Lisp -*-

(defsystem "sb-test"
  :depends-on ("sb-posix")
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-TEST;"
  :serial t
  :components
  ((:file "defpackage")

   (:file "colorize-control-codes" :if-feature (:not :win32))
   (:file "colorize-windows-console" :if-feature :win32)
   (:file "colorize")

   (:file "assertoid")
   (:file "test-util")
   (:file "compiler-test-util")

   (:file "test-funs")

   (:file "impure-runner")
   (:file "run-tests"))
  :perform (load-op :after (o c) (provide 'sb-test))
  ; :in-order-to ((test-op (test-op "sb-bsd-sockets/tests")))
  )

#+later (defsystem "sb-test/tests"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (multiple-value-bind (soft strict pending)
                 (funcall (intern "DO-TESTS" (find-package "SB-RT")))
               (declare (ignorable pending))
               (fresh-line)
               (unless strict
                 #+sb-testing-contrib
                 ;; We create TEST-PASSED from a shell script if tests passed.  But
                 ;; since the shell script only `touch'es it, we can actually create
                 ;; it ahead of time -- as long as we're certain that tests truly
                 ;; passed, hence the check for SOFT.
                 (when soft
                   (with-open-file (s #p"SYS:CONTRIB;SB-BSD-SOCKETS;TEST-PASSED.TEST-REPORT"
                                      :direction :output)
                     (dolist (pend pending)
                       (format s "Expected failure: ~A~%" pend))))
                 (warn "ignoring expected failures in test-op"))
               (unless soft
                 (error "test-op failed with unexpected failures")))))

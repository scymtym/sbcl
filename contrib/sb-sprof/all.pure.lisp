(cl:defpackage #:sb-sprof-test
  (:use #:cl #:sb-sprof #:test-util)
  (:export #:run-tests))

(cl:in-package #:sb-sprof-test)

;#+sb-fasteval (setq sb-ext:*evaluator-mode* :compile)

;;; silly examples

(defun test-0 (n &optional (depth 0))
  (declare (optimize (debug 3)))
  (when (< depth n)
    (dotimes (i n)
      (test-0 n (1+ depth))
      (test-0 n (1+ depth)))))

(with-test (:name (with-profiling :smoke))
  (let ((*standard-output* (make-broadcast-stream)))
    (sb-sprof:with-profiling (:reset t :max-samples 1000 :report :graph)
      (test-0 7))))

(defun consalot ()
  (let ((junk '()))
    (loop repeat 10000 do
         (push (make-array 10) junk))
    junk))

(with-test (:name (with-profiling :consing))
  ;; 0.0001 chosen so that it breaks rather reliably when sprof does not
  ;; respect pseudo atomic.
  (let ((*standard-output* (make-broadcast-stream)))
    (with-profiling (:reset t
                            ;; setitimer with small intervals
                            ;; is broken on FreeBSD 10.0
                            ;; And ARM targets are not fast in
                            ;; general, causing the profiling signal
                            ;; to be constantly delivered without
                            ;; making any progress.
                            #-(or freebsd arm) :sample-interval
                            #-(or freebsd arm) 0.0001
                            #+arm :sample-interval #+arm 0.1
                            :report :graph :loop nil)
      (let ((target (+ (get-universal-time) 15)))
        (princ #\.)
        (force-output)
        (loop while (< (get-universal-time) target)
           do (consalot))))))

(defun run-tests ()
  (test)
  (consing-test))

;; For debugging purposes, print output for visual inspection to see if
;; the allocation sequence gets hit in the right places (i.e. not at all
;; in traditional builds, and everywhere if SB-SAFEPOINT-STRICTLY is
;; enabled.)
#+nil (disassemble #'consalot)

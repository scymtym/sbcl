(cl:in-package "TEST-UTIL")

;;;; Result protocol and result classes

(defgeneric error-p (status))

(defgeneric failure-p (status))

(defgeneric duration (status)
  (:method ((status t))
    nil))

(defstruct test-status
  (file nil :type pathname                :read-only t)
  (name nil :type (or string symbol cons) :read-only t))

(defmethod print-object ((object test-status) stream)
  (if *print-escape*
      (call-next-method)
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~A / ~A"
                (file-namestring (test-status-file object))
                (test-status-name object)))))

(defmethod error-p ((status test-status))
  nil)

(defmethod failure-p ((status test-status))
  nil)

(defstruct (result (:include test-status))
  (start-time nil                      :type sb-kernel:internal-time :read-only t)
  (end-time   (get-internal-real-time) :type sb-kernel:internal-time :read-only t))

(defmethod duration ((status result))
  (/ (- (result-end-time status) (result-start-time status))
     internal-time-units-per-second))

(defstruct (error-result (:include result)))

(defmethod error-p ((result error-result))
  t)

(defstruct (failure-result (:include result)))

(defmethod failure-p ((result failure-result))
  t)

; (defstruct (skip-result (:include result)))

(defstruct (success
             (:include result)
             (:constructor)
             (:constructor success (file name start-time))))

(defstruct (expected-failure
             (:include result)
             (:constructor)
             (:constructor expected-failure (file name start-time condition))
             (:predicate nil)) ; TODO
  (condition nil :type (or string condition) :read-only t)) ; TODO can we avoid string?

(defstruct (unhandled-error
             (:include error-result)
             (:constructor)
             (:constructor unhandled-error (file name start-time condition)))
  (condition nil :type (or string condition) :read-only t))

(defstruct (invalid-exit-status
             (:include error-result)
             (:constructor)
             (:constructor invalid-exit-status (file name start-time exit-status)))
  (exit-status nil :type (integer 0 255) :read-only t))

(defstruct (unexpected-failure
             (:include failure-result)
             (:constructor)
             (:constructor unexpected-failure (file name start-time condition)))
  (condition nil :type (or string condition) :read-only t))

(defstruct (leftover-threads
             (:include failure-result)
             (:constructor)
             (:constructor leftover-threads (file name start-time threads)))
  (threads nil :type list :read-only t))

(defstruct (unexpected-success
             (:include failure-result)
             (:constructor)
             (:constructor unexpected-success (file name start-time))))

(defstruct (skipped-disabled ; "Test disabled for this combination of platform and features"
             (:include test-status)
             (:constructor)
             (:constructor skipped-disabled (file name))))

(defstruct (skipped-broken ; "Test broken on this platform"
             (:include test-status)
             (:constructor)
             (:constructor skipped-broken (file name))))

;;;; run

(defstruct (run
             (:constructor make-run (&optional (start-time (get-internal-real-time)))))
  (start-time nil :read-only t)
  (end-time   nil)
  (results    '() :type list))

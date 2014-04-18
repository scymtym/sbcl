(in-package :sb-bsd-sockets)

;;;

(define-condition unknown-protocol ()
  ((name :initarg :name
         :reader unknown-protocol-name))
  (:report (lambda (c s)
             (format s "Protocol not found: ~a" (prin1-to-string
                                                 (unknown-protocol-name c))))))

#+(and sb-thread (not os-provides-getprotoby-r))
;; Since getprotobyname is not thread-safe, we need a lock.
(sb-ext:defglobal **getprotoby-lock** (sb-thread:make-mutex :name "getprotoby lock"))

;;; getprotobyname only works in the internet domain, which is why this
;;; is here
(defun get-protocol-by-name (name)      ;exported
  "Given a protocol name, return the protocol number, the protocol name, and
a list of protocol aliases"

  ;; Brownie Points.  Hopefully there's one person out there using
  ;; RSPF sockets and SBCL who will appreciate the extra info
  (labels ((protoent-to-values (protoent)
             (values
              (sockint::protoent-proto protoent)
              (sockint::protoent-name protoent)
              (let ((index 0))
                (loop
                  for alias = (sb-alien:deref
                               (sockint::protoent-aliases protoent) index)
                  while (not (sb-alien:null-alien alias))
                  do (incf index)
                  collect (sb-alien::c-string-to-string
                           (sb-alien:alien-sap alias)
                           (sb-impl::default-external-format)
                           'character))))))
    #+(and sb-thread os-provides-getprotoby-r)
    (let ((buffer-length 1024)
          (max-buffer 10000)
          (result-buf nil)
          (buffer nil)
          #-solaris
          (result nil))
      (declare (type fixnum buffer-length)
               (type fixnum max-buffer))
      (loop
        (unwind-protect
             (progn
               (setf result-buf (sb-alien:make-alien sockint::protoent)
                     buffer (sb-alien:make-alien sb-alien:char buffer-length))
               #-solaris
               (setf result (sb-alien:make-alien (* sockint::protoent)))
               (when (or (sb-alien:null-alien result-buf)
                         (sb-alien:null-alien buffer)
                         (sb-alien:null-alien result))
                 (error "Could not allocate foreign memory."))
               (let ((res (sockint::getprotobyname-r
                           name result-buf buffer buffer-length #-solaris result)))
                 (cond ((eql res 0)
                        #-solaris
                        (when (sb-alien::null-alien (sb-alien:deref result 0))
                          (error 'unknown-protocol :name name))
                        (return-from get-protocol-by-name
                          (protoent-to-values result-buf)))
                       (t
                        (let ((errno (sb-unix::get-errno)))
                          (cond ((eql errno sockint::erange)
                                 (incf buffer-length 1024)
                                 (when (> buffer-length max-buffer)
                                   (error "Exceeded max-buffer of ~d" max-buffer)))
                                (t
                                 (error "Unexpected errno ~d" errno))))))))
          (when result-buf
            (sb-alien:free-alien result-buf))
          (when buffer
            (sb-alien:free-alien buffer))
          #-solaris
          (when result
            (sb-alien:free-alien result)))))
    #-(and sb-thread os-provides-getprotoby-r)
    (tagbody
       (flet ((get-it ()
                (let ((ent (sockint::getprotobyname name)))
                  (if (sb-alien::null-alien ent)
                      (go :error)
                      (return-from get-protocol-by-name (protoent-to-values ent))))))
         #+sb-thread
         (sb-thread::with-system-mutex (**getprotoby-lock**)
           (get-it))
         #-sb-thread
         (get-it))
     :error
       (error 'unknown-protocol :name name))))

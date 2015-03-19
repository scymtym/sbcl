(cl:in-package #:cl-user)

;;;; Protocol

(defgeneric walk-form (walker form kind name instead recurse
                       &rest components)
  (:documentation
   "TODO"))

(defgeneric walk-form/reconstitute (walker form kind name
                                    instead recurse reconstitute
                                    &rest components)
  (:documentation
   "TODO"))

(defgeneric on-form (hook form kind name wrap recurse &rest components)
  (:documentation
   "TODO"))

;;;; CLOS-based reconstituting identity walker

(defclass base-walker (function
                       standard-object)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance base-walker) &key)
  (sb-mop:set-funcallable-instance-function
   instance
   (lambda (instead recurse #+no reconstitute form kind name &rest components)
     (apply #'walk-form instance form kind name
            instead recurse #+no reconstitute components))))

(defmethod walk-form ((walker base-walker)
                      (form t) (kind t) (name t)
                      (instead t) (recurse t)
                      &rest components)
  (apply #'sb-c:reconstitute form kind name
         (append (funcall recurse) components)))

(defmethod walk-form ((walker base-walker)
                      (form t) (kind sb-c:macroid-info) (name t)
                      (instead t) (recurse t)
                      &rest components)
  (declare (ignore components))
  (funcall recurse))

(defmethod walk-form/reconstitute ((walker base-walker)
                                   (form t) (kind t) (name t)
                                   (instead t) (recurse t) (reconstitute t)
                                   &rest components)
  (apply reconstitute form kind name
         (append (funcall recurse) components)))

(defmethod walk-form/reconstitute ((walker base-walker)
                                   (form t) (kind sb-c:macroid-info) (name t)
                                   (instead t) (recurse t) (reconstitute t)
                                   &rest components)
  (declare (ignore components))
  (funcall recurse))

;;;; CLOS-based hooks

(defclass base-hook (function
                     standard-object)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance base-hook) &key)
  (sb-mop:set-funcallable-instance-function
   instance
   (lambda (wrap recurse form kind name &rest components)
     (apply #'on-form instance form kind name
            wrap recurse components))))

(flet ((reconstitute-and-wrap (form kind name components wrap recurse
                               &key (new-components nil new-components-p))
         (apply #'sb-c:reconstitute
                form kind name
                (append (apply recurse
                               :function (lambda (instead recurse &rest args)
                                           (declare (ignore instead recurse))
                                           (apply wrap args))
                               (when new-components-p
                                 (list :components new-components)))
                        components))))

  (defmethod on-form ((walker base-hook)
                      (form t) (kind t) (name t)
                      (wrap t) (recurse t)
                      &rest components)
    ;; Mark evaluated components (sub-forms) for later processing by
    ;; this hook.
    (reconstitute-and-wrap form kind name components wrap recurse))

  (defmethod on-form ((walker base-hook)
                      (form t) (kind sb-c:special-operator-info) (name (eql 'macrolet))
                      (wrap t) (recurse t)
                      &rest components)
    ;; Make sure to not process the bodies of the expander functions
    ;; but only the body of the MACROLET itself.
    (reconstitute-and-wrap
     form kind name wrap recurse components
     :new-components (list :body (getf components :body)))))

(defmethod on-form ((walker base-hook)
                    (form t) (kind sb-c:macroid-info) (name t)
                    (wrap t) (recurse t)
                    &rest components)
   ;; Mark macro expansion for later processing by this hook.
   (apply wrap form kind name wrap components))

;;;; Example walker

(defclass my-walker (base-walker)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod walk-form ((walker my-walker)
                      (form t) (kind sb-c:special-operator-info) (name (eql 'let))
                      (instead t) (recurse t) (reconstitute t)
                      &rest components &key names values suppliedps body)
  ;; Remove all bindings with name B.
  (loop :for name* :in names
     :for value :in (second (funcall recurse :components (list :values values)))
     :for suppliedp :in suppliedps
     :unless (eq name* 'b)
     :collect name* :into names
     :and :collect value :into values
     :and :collect suppliedp :into suppliedps
     :finally
     (return
       (apply reconstitute form kind name
              (append (list :names names :values values :suppliedps suppliedps)
                      (funcall recurse :components (list :body body))
                      components)))))

(defun walk (walker form)
  (let ((env (sb-c::coerce-to-lexenv nil)))
    (sb-c:walk-forms/reconstitute
     walker form
     (lambda (form)
       (sb-c:classify-variable-form form env))
     (lambda (form)
       (sb-c:classify-application-form form env))
     (lambda (kind expander form)
       (sb-c:expand-macro kind expander form env)))))

(print (walk (make-instance 'my-walker)
             '(let ((a (lambda (a) (let ((b 5)) b)))
                    b)
               (b (return a)))))

;;;; Example hook

(defclass my-hook (base-hook)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod on-form ((walker my-hook)
                    (form t) (kind sb-c:special-operator-info) (name (eql 'let))
                    (wrap t) (recurse t)
                    &rest components &key names values suppliedps body)
  (loop :for name* :in names
     :for value :in (second (funcall recurse :components (list :values values)))
     :for suppliedp :in suppliedps
     :unless (eq name* 'b)
     :collect name* :into names
     :and :collect value :into values
     :and :collect suppliedp :into suppliedps
     :finally
     (return
       (apply #'sb-c:reconstitute form kind name
              (append (list :names names :values values :suppliedps suppliedps)
                      (funcall recurse ; TODO same pattern here
                               :function (lambda (i r &rest args)
                                           (declare (ignore i r))
                                           (apply wrap args))
                               :components (list :body body))
                      components)))))

(defun walk/hook (form)
  ;; ENV is a hack; we never augment the environment in this example.
  (let ((env (sb-c::coerce-to-lexenv nil)))
    (sb-c:walk-forms/reconstitute
     (sb-c:add-hook-processing/reconstitute
      ;; A simple non-CLOS walk function that just expands macros and
      ;; reconstitutes everything.
      (lambda (instead recurse reconstitute form kind name &rest components)
        (declare (ignore instead))
        (typecase kind
          (sb-c:macroid-info
           (funcall recurse))
          (t
           (apply reconstitute form kind name
                  (append (funcall recurse) components))))))
     form
     (lambda (form)
       (sb-c:classify-variable-form form env))
     (lambda (form)
       (sb-c:classify-application-form form env))
     (lambda (kind expander form)
       (sb-c:expand-macro kind expander form env)))))

(print
 (walk/hook (sb-c:wrap
             (make-instance 'my-hook)
             '(let ((a (lambda (a) (let ((b 5)) b)))
                    b)
               (b (return a))))))

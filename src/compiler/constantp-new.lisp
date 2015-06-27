;;;; implementation of CONSTANTP, needs both INFO and IR1-ATTRIBUTES

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defvar *variable-values-constant*)

(defun constantp/new (form env)
  (labels ((every-component-constant-p (info components)
             (loop :for (component result) :on components :by #'cddr
                   :always (ecase (operator-component-cardinality
                                   (find component (operator-info-components info)
                                         :key #'operator-component-name))
                             ((1 ?) result)
                             ((t)   (notany #'null result)))))
           (form (instead recurse form kind name &rest components)
             (etypecase kind
               (self-evaluating-info
                t)
               (variable-info
                (destructuring-bind (&key access constant &allow-other-keys)
                    components
                  (ecase access
                    (:bind
                     (note-lexical-variable
                      name env
                      :constant (pop *variable-values-constant*))
                     t)
                    (:read
                     constant))))    ; TODO constant special variables
               (special-operator-info
                (let ((*lexenv* (make-lexenv)))
                  (case name
                    ((let let*)
                     (let ((*variable-values-constant*
                             (second (funcall recurse :components (list :values (getf components :values))))))
                       (every-component-constant-p kind (funcall recurse))))
                    (progv              ; TODO
                        (every-component-constant-p kind (funcall recurse)))
                    ((flet)
                     (destructuring-bind (&key names lambda-lists documentations local-declarations bodies &allow-other-keys) components
                       (mapcar (lambda (name lambda-list documentation local-declaration body)
                                 (note-lexical-function name lambda-list documentation local-declaration body env))
                               names lambda-lists documentations local-declarations bodies))
                     (every-component-constant-p kind (funcall recurse :components (list :body (getf components :body)))))
                    (t
                     (every-component-constant-p kind (funcall recurse))))))
               (named-application-info
                (destructuring-bind (&key where lambda-list body arguments &allow-other-keys) components
                  (case where
                    (:global
                     (awhen (info :function :info name)
                       (and (ir1-attributep (fun-info-attributes it) foldable)
                            (every-component-constant-p kind (print (funcall recurse))))))
                    (:lexical
                     (funcall instead :form `((lambda ,lambda-list ,body) ,@arguments))))))
               (lambda-application-info
                (destructuring-bind (&key lambda-list body arguments &allow-other-keys) components
                  (funcall instead :form `(let ,(mapcar #'list lambda-list arguments) ,@body))))
               (macroid-info
                (funcall recurse)))))
    (flet ((form* (instead recurse form kind name &rest components)
             #+no (print (list form kind name components))
             (fourth (print (list (type-of kind) form '=> (apply #'form instead recurse form kind name components))))))
      (let ((*free-funs* (make-hash-table))
            (*lexenv* env))
        (walk-forms 'list #'form* form
                    (lambda (form)
                      (classify-variable-form form env))
                    (lambda (form)
                      (classify-application-form form env))
                    (lambda (kind expander form)
                      (expand-macro kind expander form env)))))))

(constantp/new '(progv '(*a*) '(1 2)
                 (flet ((f (x)
                          (oddp x)))
                   (let ((b '(random)))
                     (return (let ((b 3)) (progn most-positive-fixnum (f (+ 2 b)))))
                     b)))
               (coerce-to-lexenv nil))

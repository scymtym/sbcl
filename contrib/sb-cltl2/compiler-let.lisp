(in-package :sb-cltl2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c::define-special-operator compiler-let (bindings &rest forms)
    ((:names  t :type symbol :access :bind)
     (:values t)
     (:body   t))
    #+sb-doc
    (:documentation
     "COMPILER-LET ({(var [value]) | var}*) declaration* form*

TODO")))

(sb-c::define-ir1-translator compiler-let ((names values body)
                                           instead recurse start next result)
    (progv names (mapcar #'eval values)
      (funcall recurse :components (list :body body))))

(def-ir1-translator compiler-let ((bindings &rest forms) start next result)
  (loop for binding in bindings
     if (atom binding)
        collect binding into vars
        and collect nil into values
       else do (assert (proper-list-of-length-p binding 1 2))
        and collect (first binding) into vars
        and collect (eval (second binding)) into values
     finally (return (progv vars values
                       (sb-c::ir1-convert-progn-body start next result forms)))))

(defun walk-compiler-let (form context env)
  (declare (ignore context))
  (destructuring-bind (bindings &rest body)
      (cdr form)
    (loop for binding in bindings
       if (atom binding)
          collect binding into vars
          and collect nil into values
         else do (assert (proper-list-of-length-p binding 1 2))
          and collect (first binding) into vars
          and collect (eval (second binding)) into values
       finally (return
                 (progv vars values
                   (let ((walked-body (sb-walker::walk-repeat-eval body env)))
                     (sb-walker::relist* form
                                         'compiler-let bindings walked-body)))))))

(sb-walker::define-walker-template compiler-let walk-compiler-let)

#+sb-eval
(setf (getf sb-eval::*eval-dispatch-functions* 'compiler-let)
      (lambda (form env)
        (destructuring-bind (bindings &body body) (cdr form)
          (loop for binding in bindings
                if (atom binding)
                collect binding into vars
                and collect nil into values
                else do (assert (proper-list-of-length-p binding 1 2))
                and collect (first binding) into vars
                and collect (eval (second binding)) into values
                finally (return
                          (let ((new-env (sb-eval::make-env
                                          :parent env
                                          :vars (sb-eval::special-bindings vars env))))
                            (progv vars values
                              (sb-eval::eval-progn body new-env))))))))

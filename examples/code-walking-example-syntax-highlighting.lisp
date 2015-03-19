;; Load …-with-clos.lisp first

(ql:quickload :cxml)

(cl:in-package #:cl-user)

;;;; CLOS-based "primary" walker

#+no (defclass base-walker () ())

#+no (defmethod walk-form ((walker base-walker)
                      (form t) (kind t) (name t)
                      (instead t) (recurse t)
                      &rest components)
  (let* ((new-components (funcall recurse))
         (annotated      new-components
           #+later? (loop :for (key value) :on new-components :by #'cddr
              :collect key
              :collect (list key value))))
    (log:info annotated)
    (apply #'sb-c:reconstitute form kind name
           (append annotated components))))

#+no (defmethod walk-form ((walker base-walker)
                      (form t) (kind sb-c:macroid-info) (name t)
                      (instead t) (recurse t)
                      &rest components)
  (declare (ignore components))
  (funcall recurse))

;;;; AST building walker

(defclass node ()
  ((info       :initarg  :info
               :reader   info-of)
   (name       :initarg  :name
               :reader   name-of)
   (components :initarg  :components
               :reader   components-of)
   (structure  :initarg  :structure
               :reader   structure-of)))

(defclass ast-building-walker (base-walker)
  ()
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod walk-form ((walker ast-building-walker)
                      (form t) (kind t) (name t)
                      (instead t) (recurse t)
                      &rest components &key)
  (make-instance 'node
                 :info       kind
                 :name       name
                 :components components
                 :structure  (call-next-method)))

(defmethod walk-form ((walker ast-building-walker)
                      (form t) (kind sb-c:macroid-info) (name t)
                      (instead t) (recurse t)
                      &rest components &key arguments)
  (let* ((expansion (funcall recurse :function (lambda (instead recurse form kind &rest args)
                                                 (declare (ignore instead args))
                                                 (if (typep kind 'sb-c:macro-info)
                                                     (funcall recurse)
                                                     form))))
         ;; Try to find arguments in the macroexpansion to decide
         ;; whether they are evaluated.
         ;; TODO this is still wrong we should walk the expansion and take note when we encounter argument forms in the process
         (arguments (mapcar (lambda (argument)
                              (block nil
                                (funcall instead
                                         :function (lambda (instead1 recurse form &rest args)
                                                     (declare (ignore instead1 args))
                                                     (when (eq argument form)
                                                       (log:info argument "in" expansion)
                                                       (return-from nil
                                                         (funcall instead :form argument)))
                                                     (funcall recurse))
                                         :form     expansion)
                                argument))
                            arguments))
         (structure (apply #'sb-c:reconstitute form kind name
                           (list* :arguments arguments components))))
    (log:info expansion arguments)
    (make-instance 'node
                   :info       kind
                   :name       name
                   :components components
                   :structure  structure)))

(defun form->tree (form)
  (let ((walker (make-instance 'ast-building-walker)))
    (handler-bind
        ((sb-c::invalid-form-error
          (lambda (condition)
            (use-value `(invalid-form
                         ',(sb-c::invalid-form-error-form ; TODO export
                            condition))
                       condition))))
      (let* ((env (sb-c::coerce-to-lexenv nil))
             (sb-c:*lexenv* env))
        (sb-c:walk-forms
         'list
         (lambda (instead recurse form kind name &rest components)
           (apply #'walk-form walker
                  form kind name
                  instead recurse
                  components))
         form
         (lambda (form)
           (sb-c:classify-variable-form form env))
         (lambda (form)
           (sb-c:classify-application-form form env))
         (lambda (kind expander form)
           (sb-c:expand-macro kind expander form env)))))))

(form->tree '(alexandria:when-let ((a 1)) a))

;;; Rendering

(defvar *indent* 0)

(labels ((space ()
           (cxml:unescaped "&nbsp;"))
         (call-with-parens (thunk)
           (cxml:with-element "span"
             (cxml:attribute "class" "paren")
             (cxml:text "("))
           (funcall thunk)
           (cxml:with-element "span"
             (cxml:attribute "class" "paren")
             (cxml:text ")")))
         (do-children (children &optional indentation)
           (let ((*indent* *indent*))
             (loop :for (sub rest) :on children
                :for i :from 0 :do
                (log:info i indentation)
                (when (eql i indentation)
                  (incf *indent*))
                (when (and indentation (>= i indentation))
                  (cxml:with-element "br")
                  (dotimes (i *indent*) (space)))
                (bla sub)
                (when rest (space)))))
         (bla (form)
           (typecase form
             (cons
              (call-with-parens (alexandria:curry #'do-children form)))
             (node
              (render form))
             (t
              (cxml:text (prin1-to-string form)))))

         (calloid (name structure &optional indentation)
           (assert (eq name (first structure)))
           (call-with-parens
            (lambda ()
              (cxml:with-element "a"
                (cxml:attribute "class" "head")
                (cxml:attribute "href"
                                (cond
                                  ((not (symbol-package name)) ; TODO no link in this case
                                   "")
                                  ((eq (symbol-package name) (find-package :common-lisp))
                                   (format nil "http://l1sp.org/cl/~(~A~)"
                                           (symbol-name name)))
                                  (t
                                   (format nil "~A/~A"
                                           (package-name (symbol-package name))
                                           (symbol-name name)))))
                (cxml:text (princ-to-string name)))
              (alexandria:when-let ((children (rest structure)))
                (space)
                (do-children children indentation))))))

  (defmethod render-using-info ((node t) (info t) (name t)
                                &key)
    (bla (structure-of node)))

  (defmethod render-using-info ((node t) (info sb-c:named-application-info) (name t)
                                &key)
    (calloid name (structure-of node)))

  (defmethod render-using-info ((node t) (info sb-c:compiler-macro-info) (name t)
                                &key)
    (calloid name (structure-of node)))

  (defmethod render-using-info ((node t) (info sb-c:special-operator-info) (name t)
                                &key)
    (calloid name (structure-of node)))

  (defmethod render-using-info ((node t) (info sb-c:special-operator-info) (name (eql 'progn))
                                &key)
    (calloid name (structure-of node) 0))

  (defmethod render-using-info ((node t) (info sb-c:macro-info) (name t)
                                &key)
    (log:info (sb-pretty::macro-indentation name))
    (calloid name (structure-of node) (sb-pretty::macro-indentation name)))

  (Defmethod render-using-info :around ((node t) (info sb-c:named-application-info) (name (eql 'invalid-form))
                                        &key)
    (cxml:with-element "span"
      (cxml:attribute "class" "error")
      (bla (second (structure-of (second (structure-of node)))))))) ; TODO ugly/explain

(defmethod render-using-info :around ((node t) (info t) (name t) &key)
  (let* ((kind        (string-downcase (type-of info)))
         (package     (when (symbolp name)
                        (alexandria:when-let ((package (symbol-package name)))
                          (package-name package))))
         (symbol-name (when (symbolp name)
                        (string name)))
         (classes     (list* kind name
                             (when (typep info 'sb-c:leaf-info)
                               (list (typecase name
                                       (string 'string)
                                       (number 'number)
                                       (t      (type-of name))))))))
    (cxml:with-element "span"
      (cxml:attribute "class" (format nil "~(~{~A~^ ~}~)" classes))
      (cxml:attribute "form-kind" kind)
      (when package
        (cxml:attribute "form-name-package" package))
      (when name
        (cxml:attribute "form-name-name" symbol-name))
      (call-next-method))))

(defmethod render ((node t))
  (apply #'render-using-info node (info-of node) (name-of node)
         (components-of node)))

(defun call-with-html-output (stream thunk)
  (cxml:with-xml-output (cxml:make-character-stream-sink stream)
    (cxml:with-element "html"
      (cxml:with-element "head"
        (cxml:with-element "link"
          (cxml:attribute "rel" "stylesheet")
          (cxml:attribute "href" "bla.css")))
      (cxml:with-element "body"
        (let ((*readtable* (copy-readtable)))
          (setf (readtable-case *readtable*) :invert)
          (funcall thunk))))))

(let ((form

       '(labels ((space ()
                  (cxml:unescaped "&nbsp;"))
                 (call-with-parens (thunk)
                  (cxml:with-element "span"
                    (cxml:attribute "class" "paren")
                    (cxml:text "("))
                  (funcall thunk)
                  (cxml:with-element "span"
                    (cxml:attribute "class" "paren")
                    (cxml:text ")")))
                 (do-children (children &optional (indentation 1))
                   (let ((*indent* *indent*))
                     (loop :for (sub rest) :on children
                        :for i :from 0 :do
                        (log:info i indentation)
                        (when (and indentation (>= i indentation))
                          (cxml:with-element "br")
                          (incf *indent*)
                          (dotimes (i *indent*) (space)))
                        (bla sub)
                        (when rest (space)))))
                 (bla (form)
                  (typecase form
                    (cons
                     (call-with-parens (alexandria:curry #'do-children form)))
                    (node
                     (render form))
                    (t
                     (cxml:text (prin1-to-string form)))))

                 (calloid (name structure &optional (indentation 1))
                  (assert (eq name (first structure)))
                  (call-with-parens
                   (lambda ()
                     (cxml:with-element "a"
                       (cxml:attribute "class" "head")
                       (cxml:attribute "href"
                                       (cond
                                         ((eq (symbol-package name) (find-package :common-lisp))
                                          (format nil "http://l1sp.org/cl/~(~A~)"
                                                  (symbol-name name)))
                                         (t
                                          (format nil "~A/~A"
                                                  (package-name (symbol-package name))
                                                  (symbol-name name)))))
                       (cxml:text (princ-to-string name)))
                     (alexandria:when-let ((children (rest structure)))
                       (space)
                       (do-children children indentation))))))

         (defmethod render-using-info ((node t) (info t) (name t)
                                       &key)
           (bla (structure-of node)))
         (defmethod render-using-info ((node t) (info sb-c:named-application-info) (name t)
                                       &key)
           (calloid name (structure-of node)))
         (defmethod render-using-info ((node t) (info sb-c:compiler-macro-info) (name t)
                                       &key)
           (calloid name (structure-of node)))
         (defmethod render-using-info ((node t) (info sb-c:special-operator-info) (name t)
                                       &key)
           (calloid name (structure-of node)))
         (defmethod render-using-info ((node t) (info sb-c:special-operator-info) (name (eql 'progn))
                                       &key)
           (calloid name (structure-of node) 0))
         (defmethod render-using-info ((node t) (info sb-c:macro-info) (name t)
                                       &key)
           (log:info (sb-pretty::macro-indentation name))
           (calloid name (structure-of node) (or (sb-pretty::macro-indentation name) 0)))
         (defmethod render-using-info :around ((node t) (info sb-c:named-application-info) (name (eql 'invalid-form))
                                               &key)
           (cxml:with-element "span"
             (cxml:attribute "class" "error")
             (bla (second (structure-of (second (structure-of node))))))))
       #+no`(defmethod foo ((a bar))
          (cxml:with-element "bla"
            (flet ((foo (x &key bar)
                     (list :bar 1 2)))
              (let ((a 1) (b 2))
                (foo ((a b)))
                "foo"))))

        ))

  (let ((tree (form->tree form)))
    (alexandria:with-output-to-file (stream "/tmp/bla.html" :if-exists :supersede)
      (call-with-html-output
       stream (lambda () (render tree))))))

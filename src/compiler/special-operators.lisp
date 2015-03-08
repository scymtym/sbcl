;;;; Special operators and machinery for defining them

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; TODO This file only contains original code. Still derived from CMU note?

(cl:in-package "SB!C")


;;;; OPERATOR-{COMPONENT,INFO}

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Syntax-level information about a syntactic component of a
;;; (special) operator. For example, the special operator (return-from
;;; BLOCK-NAME VALUE) has the two components BLOCK-NAME and VALUE.
(defstruct (operator-component
             (:constructor make-operator-component
                           (name cardinality evaluated &key type))
             (:copier nil))
  ;; Name of the component. Usually a keyword. Only needs to be unique
  ;; within the special operator.
  (name        nil :type symbol         :read-only t)
  ;; Number of elements in the component. One of
  ;; ? -> Zero or one element
  ;;      Example: VALUE in (return-from BLOCK-NAME VALUE)
  ;; 1 -> One element
  ;;      Example: BLOCK-NAME in (return-from BLOCK-NAME VALUE)
  ;; t -> A list of elements
  ;;      Example: FORMS in (progn FORMS)
  (cardinality nil :type (member ? 1 t) :read-only t)
  ;; Indicates whether the component is evaluated when the special
  ;; operator is evaluated.
  ;;
  ;; Example: in (return-from BLOCK-NAME VALUE), BLOCK-NAME is not
  ;; evaluated but VALUE is evaluated.
  (evaluated   nil :type boolean        :read-only t)
  ;; A type restriction for the component. For evaluated component,
  ;; the restriction applies to the component before evaluation.
  ;;
  ;; For components with cardinality T, every element must satisfy the
  ;; type restriction.
  ;;
  ;; Example: in (return-from BLOCK-NAME VALUE), BLOCK-NAME must be of
  ;; type SYMBOL.
  (type        t                        :read-only t))

(defstruct (operator-access-component
             (:include operator-component)
             (:constructor make-operator-access-component
                           (name cardinality evaluated &key type access))
             (:copier nil))
  (access nil :type (member nil :read :write :bind) :read-only t))

;; TODO alternative: two subclasses EVALUATED-COMPONENT, UNEVALUATED-COMPONENT

;;; Syntax-level information about an operator.
(defstruct (operator-info
             (:copier nil))
  ;; A description the syntactic components of the special operator
  ;; including whether they are evaluated (OPERATOR-COMPONENT
  ;; instances).
  ;;
  ;; Elements have to appear in an order that is compatible with
  ;; evaluation order of the special operator. Elements corresponding
  ;; to unevaluated components can theoretically appear at any
  ;; position but should be grouped for ease of understanding.
  (components '() :type list   :read-only t))

) ; eval-when

(defstruct (special-operator-info
             (:include operator-info)
             (:constructor make-special-operator-info
                           (name components parser unparser))
             (:copier nil))
  ;; Name of the special operator.
  (name     nil :type symbol :read-only t)
  ;; How to parse a form headed by the special operator and decompose
  ;; it into components. Semantic of these components: binding names,
  ;; forms that will be evaluated, etc.
  ;; TODO describe lambda-list
  ;;
  ;; Parser functions must report components in the reverse order of
  ;; the order defined by the COMPONENTS slot.
  (parser   nil :type function :read-only t)
  ;; Unparse components into a form headed by the special operator.
  ;; TODO describe lambda-list
  (unparser nil :type function :read-only t))

;;; Return the SPECIAL-OPERATOR-INFO instance for the special operator
;;; designated by NAME or NIL if there is no such special operator.
#+TODO (declaim (ftype (sfunction (symbol) (or null special-operator-info))
                find-special-operator-info))
(defun find-special-operator-info (name)
  (declare (notinline info)) ; TODO why is this necessary?
  (values (info :function :special-operator-info name)))

;;; Install NEW-VALUE, a SPECIAL-OPERATOR-INFO instance for the
;;; special operator designated by NAME.
#+TODO (declaim (ftype (sfunction (special-operator-info symbol) special-operator-info)
                (setf find-special-operator-info)))
(defun (setf find-special-operator-info) (new-value name)
  (declare (notinline info (setf info))) ; TODO why is this necessary?
  (multiple-value-bind (old oldp)
      (info :function :special-operator-info name) ; TODO do this atomically
    (when oldp
      (cerror "Replace the existing special operator definition"
              "~@<~S already names the special operator info ~A~@:>"
              name old)))
  (setf (info :function :special-operator-info name) new-value))

;;;; "runtime" support functions

(defun check-component-type/1 (operator-name component-name value type)
  (if (typep value type)
      value
      (compiler-error
       "~@<~S special operator component ~A is ~S which is not of type ~
        ~S.~@:>"
       operator-name component-name value type)))

(defun check-component-type/t (operator-name component-name value type)
  (let ((offender (position-if (lambda (element)
                                 (not (typep element type)))
                               value)))
    (if offender
        (compiler-error
         "~@<~S special operator component ~A element ~S is not of ~
          type ~S.~@:>"
         operator-name component-name (nth offender value) type)
        value)))

;;;; DEFINE-SPECIAL-OPERATOR macro

;; TODO is this EVAL-WHEN wrong? It produces redefinition warnings
;; during x-compilation.
(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Return a list of forms each of which creates a OPERATOR-COMPONENT
;;; instance according to the corresponding component description in
;;; COMPONENTS which is of the form
;;;
;;;   (name cardinality &key evaluated type access)
(defun make-special-operator-component-forms (components)
  (flet ((make-component-form (name cardinality
                               &key (evaluated 't) type access)
           `(,(if access
                  'make-operator-access-component
                  'make-operator-component)
             ',name ',cardinality ,evaluated
             ,@(when type `(:type ',type))
             ,@(when access `(:access ',access)))))
    (mapcar (lambda (spec) (apply #'make-component-form spec))
            components)))

;;; TODO explain
(defun split-special-operator-components (lambda-list components)
  (flet ((split-at (position list)
           (values (subseq list 0 position)
                   (subseq list position))))
    (binding* (((nil required optional restp) (parse-lambda-list lambda-list))
               ((required-components rest)
                (split-at (length required) components))
               ((optional-components rest)
                (split-at (length optional) rest))
               (rest-components
                (if restp
                    (split-at 1 rest)
                    (values () rest))))
      (values required-components optional-components
              (when rest-components (first rest-components))))))

(defun make-special-operator-parser-names (name)
  (values (symbolicate "PARSE-" name "-SPECIAL-OPERATOR") ; TODO suffix can go away later
          (symbolicate "UNPARSE-" name "-SPECIAL-OPERATOR")))

(defun make-special-operator-parser-body
    (name lambda-list whole-var components body)
  (flet ((make-clause (component value-var)
           (destructuring-bind (name* cardinality
                                &key (type nil typep) &allow-other-keys)
               component
             `(,name*
               ,(cond
                 ((not typep)
                  value-var)
                 ((or (eql '? cardinality) (eql 1 cardinality))
                  ``(check-component-type/1
                     ',',name ',',name* ,,value-var ',',type))
                 ((eql t cardinality)
                  ``(check-component-type/t
                     ',',name ',',name* ,,value-var ',',type)))))))
    `((macrolet ((%component (name value)
                   (case name
                     ,@(mapcar #'make-clause components '#1=(value . #1#))
                     (t
                      (error "~@<Unknown component: ~S.~@:>" name)))))
        (named-ds-bind (:special-form ,name) ,lambda-list (rest ,whole-var)
          ,@body)))))

(defun make-special-operator-explicit-parser-body
    (name lambda-list cont-var whole-var components body)
  (make-special-operator-parser-body
   name lambda-list whole-var components
   `((let ((components '()))
       (macrolet ((component (name value)
                    `(setf components
                           (list* ,name (%component ,name ,value)
                                  components))))
         ,@body
         (apply ,cont-var components))))))

(defun make-special-operator-default-parser-body
    (name lambda-list cont-var whole-var components)
  (flet ((make-component (parameter component)
           (let ((name (first component)))
             `(,name (%component ,name ,parameter))))
         (required (spec)
           spec)
         (optional (spec)
           (destructuring-bind (name value) spec
             `((when ,value (list ,name ,value))))))
    (binding* (((nil required-parameters optional-parameters rest-parameter)
                (parse-lambda-list lambda-list))
               (rest-parameter (first rest-parameter)) ; TODO ugly
               ((required-components optional-components rest-component)
                (split-special-operator-components lambda-list components))
               (required (mapcar #'make-component required-parameters required-components))
               (optional (mapcar #'make-component optional-parameters optional-components))
               (rest (when rest-parameter
                       (make-component rest-parameter rest-component)))
               (body (cond
                       ((not (or optional-parameters rest-parameter))
                        `((funcall ,cont-var ,@(mapcan #'required required))))
                       ((and (not optional-parameters) rest-parameter)
                        `((apply ,cont-var ,@(mapcan #'required required) ,@(optional rest)))) ; TODO
                       ((and (= (length optional-parameters) 1) (not rest-parameter))
                        `((apply ,cont-var
                                 ,@(mapcan #'required required)
                                 ,@(optional (first optional)))))
                       ((and optional-parameters (not rest-parameter))
                        `((apply ,cont-var
                                 ,@(mapcan #'required required)
                                 (append ,@(mapcan #'optional optional)))))
                       (t
                        `((apply ,cont-var
                                 ,@(mapcan #'required required)
                                 (append ,@(mapcan #'optional optional)
                                         ,@(optional rest))))))))
      (make-special-operator-parser-body
       name lambda-list whole-var components body))))

(defun make-special-operator-unparser-lambda-list (components)
  (collect ((parameters) (supplied))
    (flet ((make-parameter (spec)
             (destructuring-bind (name cardinality &key &allow-other-keys)
                 spec
               (with-unique-names (parameter suppliedp)
                 (parameters (list name parameter))
                 (ecase cardinality
                   ((1 t)
                    `((,name ,parameter)))
                   (?
                    (supplied (list name suppliedp))
                    `((,name ,parameter) nil ,suppliedp)))))))
      (values `(&key ,@(mapcar #'make-parameter components))
              (parameters) (supplied)))))

;; TODO common unparser stuff + explicit and default functions
(defun make-special-operator-unparser-body (name components body)
  (binding* (((lambda-list parameters supplied)
              (make-special-operator-unparser-lambda-list components)))
    (flet ((make-case-clause (pair)
             `(,(first pair) ',(second pair))))
      (values lambda-list
              `((macrolet ((component (name)
                             (case name
                               ,@(mapcar #'make-case-clause parameters)))
                           (componentp (name)
                             (case name
                               ,@(mapcar #'make-case-clause supplied)
                               (t
                                (error "~@<~S is a required component.~@:>"
                                       name)))))
                  (list* ',name ,@body)))))))

(defun make-special-operator-default-unparser-body (name lambda-list components)
  (flet ((required (spec)
           `(component ,(first spec)))
         (optional (spec)
           `(when (componentp ,(first spec))
              (list (component ,(first spec))))))
    (multiple-value-bind (required-components optional-components rest-component)
        (split-special-operator-components lambda-list components)
      (make-special-operator-unparser-body
       name components
       (append (mapcar #'required required-components)
               (cond
                 ((and optional-components rest-component)
                  `((append ,@(mapcar #'optional optional-components)
                            ,(required rest-component))))
                 ((= (length optional-components) 1)
                  `(,(optional (first optional-components))))
                 (optional-components
                  `((append ,@(mapcar #'optional optional-components))))
                 (rest-component
                  `(,(required rest-component)))
                 (t
                  '(()))))))))

(defun make-special-operator-expansion (name lambda-list components
                                        &key
                                        (parser        nil parser-supplied-p)
                                        (unparser      nil unparser-supplied-p)
                                          (documentation nil documentationp))
  ;; Basic syntax checks.
  (dolist (component components)
    (destructuring-bind (name cardinality &key &allow-other-keys)
        component
      (declare (ignore name))
      (check-type cardinality (member ? 1 t))))
  (when documentationp
    (check-type documentation (cons string)))
  ;; Expansion.
  (with-unique-names (cont-var whole-var)
    (binding* ((component-forms
                (make-special-operator-component-forms components))
               ((parser-name unparser-name)
                (make-special-operator-parser-names name))
               (parser-body
                (if parser-supplied-p
                    (make-special-operator-explicit-parser-body
                     name lambda-list cont-var whole-var components parser)
                    (make-special-operator-default-parser-body
                     name lambda-list cont-var whole-var components)))
               ((unparser-lambda-list unparser-body)
                (if unparser-supplied-p
                    (make-special-operator-unparser-body
                     name components unparser)
                    (make-special-operator-default-unparser-body
                     name lambda-list components))))
      `(progn
         (defun ,parser-name (,cont-var ,whole-var)
           ,@parser-body)

         (defun ,unparser-name ,unparser-lambda-list
           ,@unparser-body)

         ;; TODO needs adjustments where :kind is still expected to be :special-form
         #+later (setf (info :function :kind ',name) :special-operator)

         #-sb-xc-host
         ;; It's nice to do this for error checking in the target
         ;; SBCL, but it's not nice to do this when we're running in
         ;; the cross-compilation host Lisp, which owns the
         ;; SYMBOL-FUNCTION of its COMMON-LISP symbols. These guard
         ;; functions also provide the documentation for special forms.
         ;; FIXME: should be disallowed after bootstrap. Package-lock
         ;; prevents it, but the protection could be stronger than that.
         ;; The lambda name has significance to COERCE-SYMBOL-TO-FUN.
         (when nil ; TODO later
           (let ((fun (named-lambda (sb!impl::special-operator ,name) ; TODO make a function make-special-operator-stub?
                          (&rest args)
                        ,@(when documentation documentation)
                        (declare (ignore args))
                        (error 'special-form-function :name ',name))))
             ;; Set up a macro lambda list to a function.
             (setf (%simple-fun-arglist fun)
                   ',(if (eq (first lambda-list) '&whole)
                         (cddr lambda-list)
                         lambda-list)
                   (symbol-function ',name) fun)))

         ;;
         (setf (find-special-operator-info ',name)
               (make-special-operator-info
                ',name (list ,@component-forms)
                #',parser-name #',unparser-name))
         ',name))))

) ; eval-when

;;; COMPONENTS are of the form
;;;
;;;   (NAME CARDINALITY &key type evaluated)
;;;
;;; where NAME is a keyword naming the syntactic component,
;;; CARDINALITY is one of
;;;
;;;   ? The component consists of TODO
;;;   1 The component consists of
;;;   T The component consists of
;;;
;;; EVALUATED indicates whether the component is intended for
;;; evaluation. TYPE can be used to restrict things that can appear in
;;; the component.
;;;
;;; Parser body can call
;;;
;;;   (component name value)
;;;
;;; to indicate that the component named NAME is VALUE in the form.
;;;
;;; For simple cases, :PARSER and :UNPARSER can be omitted and will be
;;; generated automatically.
;;;
;;; Slightly complicated example: TODO outdated
;;;
;;;   (define-special-operator let (bindings &body body)
;;;       ((:names        t :evaluated nil :type symbol)
;;;        (:inits        t :evaluated t)
;;;        (:declarations t :evaluated nil)
;;;        (:body         t :evaluated t))
;;;     (:parser
;;;      (multiple-value-bind (body declarations)
;;;          (parse-body body :doc-string-allowed nil)
;;;        (component :names        (mapcar #'first  bindings))
;;;        (component :inits        (mapcar #'second bindings))
;;;        (component :declarations declarations)
;;;        (component :body         body)))
;;;     (:unparser
;;;      `(,(mapcar #'list (component :names) (component :inits))
;;;        ,@(component :declarations)
;;;        ,@(component :body)))
;;;
(defmacro define-special-operator (name lambda-list components
                                   &rest options)
  (apply #'make-special-operator-expansion
         name lambda-list components
         (loop :for (key . rest) :in options :collect key :collect rest)))

;;;; Special operators
;;;;
;;;; TODO remove later: The order of operators is according to
;;;; src/compiler/ir1-translators.lisp except that SYMBOL-MACROLET and
;;;; MACROLET have been moved to LET, LET* and FLET, LABELS
;;;; respectively.
;;;;
;;;; Names in lambda-lists and components should be according the
;;;; respective clhs entry (if there is one), but probably are not all
;;;; cases.


;;;; Special operators for control

(define-special-operator progn (&rest forms)
  ((:forms t))
  #!+sb-doc
  (:documentation
   "PROGN form*

Evaluates each FORM in order, returning the values of the last
form. With no forms, returns NIL."))

(define-special-operator if (test then &optional else)
  ((:test 1)
   (:then 1)
   (:else ?))
  #!+sb-doc
  (:documentation
   "IF test then [else]

If TEST evaluates to true, evaluate THEN and return its values,
otherwise evaluate ELSE and return its values. ELSE defaults to
NIL."))

;;;; BLOCK, RETURN-FROM and TAGBODY

(define-special-operator block (name &rest forms)
  ((:name  1 :evaluated nil :type symbol)
   (:forms t))
  #!+sb-doc
  (:documentation
   "BLOCK name form*

Evaluate the FORMS as a PROGN. Within the lexical scope of the body,
RETURN-FROM can be used to exit the form."))

(define-special-operator return-from (name &optional value)
  ((:name  1 :evaluated nil :type symbol)
   (:value ?))
  #!+sb-doc
  (:documentation
   "RETURN-FROM block-name value-form

Evaluate the VALUE-FORM, returning its values from the lexically
enclosing block BLOCK-NAME. This is constrained to be used only within
the dynamic extent of the block."))

(define-special-operator tagbody (&rest body)
  ((:tags     t :evaluated nil :type (or integer symbol))
   (:segments t))
  #!+sb-doc
  (:documentation
   "TAGBODY {tag | statement}*

Define tags for use with GO. The STATEMENTS are evaluated in order,
skipping TAGS, and NIL is returned. If a statement contains a GO to a
defined TAG within the lexical scope of the form, then control is
transferred to the next statement following that tag. A TAG must be an
integer or a symbol. A STATEMENT must be a list. Other objects are
illegal within the body.")
  (:parser
   ;; Collect tags and segments in an alternating manner. We must
   ;; allow empty segments to not get confused by adjacent tags.
   (collect ((tags) (segments))
     (let ((current body))
       (loop
          (let ((next-segment (member-if #'atom current)))
            (unless next-segment
              (segments `(progn ,@current))
              (return))
            (let ((tag (car next-segment)))
              (when (member tag (tags))
                (compiler-error
                 "~@<The tag ~S appears more than once in a ~S.~@:>"
                 tag 'tagbody))
              (tags tag)
              (segments `(progn ,@(ldiff current next-segment))))
            (setf current (rest next-segment)))))
     (component :tags     (tags))
     (component :segments (segments))))
  (:unparser
   ;; Temporarily prepend a NIL tag for ease of implementation.
   (rest (mapcan (lambda (tag segment)
                   `(,tag ,@(if (typep segment '(cons (eql progn)))
                                (rest segment)
                                (list segment))))
                 (list* nil (component :tags))
                 (component :segments)))))

(define-special-operator go (tag)
  ((:tag 1 :evaluated nil :type symbol))
  #!+sb-doc
  (:documentation
   "GO tag

Transfer control to the named TAG in the lexically enclosing
TAGBODY. This is constrained to be used only within the dynamic extent
of the TAGBODY."))

;;;; Compiler-magic special forms

(defconstant-eqx +eval-when-situations+
    '(:compile-toplevel compile
      :load-toplevel    load
      :execute          eval)
  #'equal)

(deftype eval-when-situation ()
    `(member ,@+eval-when-situations+))

(define-special-operator eval-when (situations &rest forms)
  ((:situations t :evaluated nil :type eval-when-situation)
   (:forms      t))
  #!+sb-doc
  (:documentation
   "EVAL-WHEN (situation*) form*

Evaluate the FORMS in the specified SITUATIONS (any
of :COMPILE-TOPLEVEL, :LOAD-TOPLEVEL, or :EXECUTE, or (deprecated)
COMPILE, LOAD, or EVAL)."))

(define-special-operator %primitive (name &rest args)
  ((:name 1 :evaluated nil :type symbol)
   (:args t)))

(define-special-operator quote (thing)
  ((:thing 1 :evaluated nil))
  #!+sb-doc
  (:documentation
   "QUOTE value

Return VALUE without evaluating it."))

(define-special-operator %%allocate-closures (&rest leaves)
  ((:leaves t)))

(define-special-operator function (thing)
  ((:name          ? :evaluated nil)
   (:lambda-list   ? :evaluated nil)
   (:declarations  t :evaluated nil)
   (:documentation ? :evaluated nil)
   (:body          t))
  #!+sb-doc
  (:documentation
   "FUNCTION name

Return the lexically apparent definition of the function NAME. NAME
may also be a lambda expression.")
  (:parser
   (cond
     ((legal-fun-name-p thing)
      (component :name thing))
     ((typep thing '(cons (eql lambda) (cons list)))
      ;; Components in reverse evaluation-order.
      (multiple-value-bind (body declarations documentation)
          (parse-body (nthcdr 2 thing))
        (component :body body)
        (when documentation
          (component :documentation documentation))
        (component :declarations declarations))
      (component :lambda-list (second thing)))
     (t
      (compiler-error "~@<Invalid argument to ~S special operator: ~
                       ~S.~@:>"
                      'function thing))))
  (:unparser
   (list (cond
           ((componentp :name)
            (component :name))
           ((componentp :lambda-list)
            `(lambda ,(component :lambda-list)
               ,@(component :declarations)
               ,@(when (componentp :documentation)
                   `(,(component :documentation)))
               ,@(component :body)))
           (t
            (error "~@<Exactly one of ~S and ~S must be supplied.~@:>"
                   :name :lambda-list))))))

(define-special-operator global-function (thing)
  ((:thing 1 :evaluated nil)))

(define-special-operator %funcall (function &rest args)
  ((:function 1 :evaluated nil) ; TODO type (or (cons (member function global-function) ?)
   (:args     t)))

;;;; SYMBOL-MACROLET, LET[*] and LOCALLY

(defun parse-variable-like-bindings (context bindings binding-parser)
  (collect ((names) (value-forms) (suppliedps))
    (dolist (spec bindings (values (names) (value-forms) (suppliedps)))
      (multiple-value-bind (name value suppliedp)
          (funcall binding-parser context spec)
        (names name)
        (value-forms value)
        (suppliedps suppliedp)))))

(defun parse-symbol-macrolet-binding (context spec)
  (unless (proper-list-of-length-p spec 2)
    (compiler-error "~@<Malformed symbol-expansion pair in ~S: ~S~@:>"
                    context spec)) ; TODO should be compiler-error or simple-program-error depending on the context
  (values (check-variable-name (first spec) "local symbol-macro name")
          (second spec)))

(defun parse-let-binding (context spec)
  (multiple-value-bind (name value suppliedp)
      (cond
        ((atom spec)
         spec)
        ((not (proper-list-of-length-p spec 1 2))
         (compiler-error "~@<The ~S binding spec ~S is malformed.~@:>"
                         context spec))
        (t
         (values (first spec) (second spec) t)))
    (values (check-variable-name name "local variable") value suppliedp)))

(defun unparse-let-binding (name value suppliedp)
  (if suppliedp
      (list name value)
      name))

(macrolet ((define-let-special-operator
               (name values-keyword
                bindings-parser bindings-unparser suppliedp-p
                &optional documentation)
             `(define-special-operator ,name (bindings &body body)
                ((:names          t :type symbol :access :bind)
                 (,values-keyword t)
                 ,@(when suppliedp-p
                     `((:suppliedps     t :evaluated nil)))
                 (:declarations   t :evaluated nil)
                 (:body           t))
                ,@(when documentation
                    `((:documentation ,documentation)))
                (:parser
                 (binding* (((forms declarations)
                             (parse-body body :doc-string-allowed nil))
                            ((names values ,@(when suppliedp-p '(supplieds)))
                             (parse-variable-like-bindings
                              ',name bindings #',bindings-parser)))
                   ;; Components in reverse evaluation-order.
                   (component :body           forms)
                   (component :declarations   declarations)
                   ,(when suppliedp-p
                      '(component :suppliedps      supplieds))
                   (component ,values-keyword values)
                   (component :names          names)))
                (:unparser
                 `(,(mapcar #',bindings-unparser
                            (component :names)
                            (component ,values-keyword)
                            ,@(when suppliedp-p
                                '((component :suppliedps))))
                    ,@(component :declarations)
                    ,@(component :body))))))

  (define-let-special-operator symbol-macrolet
    :expansions parse-symbol-macrolet-binding list nil
    #!+sb-doc
    "SYMBOL-MACROLET ({(name expansion)}*) decl* form*

Define the NAMES as symbol macros with the given EXPANSIONS. Within the
body, references to a NAME will effectively be replaced with the EXPANSION.")

  (define-let-special-operator let
    :values parse-let-binding unparse-let-binding t
    #!+sb-doc
    "LET ({(var [value]) | var}*) declaration* form*

During evaluation of the FORMS, bind the VARS to the result of
evaluating the VALUE forms. The variables are bound in parallel after
all of the VALUES forms have been evaluated.")

  (define-let-special-operator let*
    :values parse-let-binding unparse-let-binding t
    #!+sb-doc
    "LET* ({(var [value]) | var}*) declaration* form*

Similar to LET, but the variables are bound sequentially, allowing
each VALUE form to reference any of the previous VARS."))

(define-special-operator locally (&rest body)
  ((:declarations t :evaluated nil)
   (:body         t))
  #!+sb-doc
  (:documentation
   "LOCALLY declaration* form*

Sequentially evaluate the FORMS in a lexical environment where the
DECLARATIONS have effect. If LOCALLY is a top level form, then the
FORMS are also processed as top level forms.")
  (:parser
   (multiple-value-bind (forms declarations)
       (parse-body body :doc-string-allowed nil)
     ;; Components in reverse evaluation-order.
     (component :body         forms)
     (component :declarations declarations)))
  (:unparser
   (append (component :declarations) (component :body))))

;;;; MACROLET, FLET and LABELS

(defun parse-function-like-bindings (context bindings binding-parser)
  (collect ((names) (lambda-lists) (docs) (declarations) (bodies))
    (dolist (spec bindings (values (names) (lambda-lists)
                                   (docs) (declarations) (bodies)))
      (multiple-value-bind (name lambda-list body declarations doc)
          (funcall binding-parser context spec)
        (names name)
        (lambda-lists lambda-list)
        (docs doc)
        (declarations declarations)
        (bodies body)))))

(defun parse-flet-binding (context spec)
  (cond
    ((or (atom spec) (< (length spec) 2)) ; TODO handle circularity, improper lists
     (compiler-error "~@<The ~S definition spec ~S is malformed.~@:>"
                     context spec))
    (t
     (destructuring-bind (name lambda-list &rest body) spec
       (multiple-value-bind (body declarations documentation)
           (parse-body body)
         (values (check-fun-name name) lambda-list
                 `(progn ,@body) declarations documentation))))))

(defun unparse-flet-binding (name lambda-list documentation declarations body)
  `(,name ,lambda-list
    ,@(when documentation `(,documentation))
    ,@declarations
    ,@(if (typep body '(cons (eql progn)))
          (rest body)
          (list body))))

(macrolet ((define-flet-special-operator
               (name binding-parser &optional documentation)
             `(define-special-operator ,name (definitions &body body)
                  ((:names              t :evaluated nil :type symbol)
                   (:lambda-lists       t :evaluated nil :type list)
                   (:documentations     t :evaluated nil :type (or null string))
                   (:local-declarations t :evaluated nil)
                   (:bodies             t) ; TODO local-bodies?
                   (:declarations       t :evaluated nil)
                   (:body               t))
                ,@(when documentation
                    `((:documentation ,documentation)))
                (:parser
                 (binding* (((forms declarations)
                             (parse-body body :doc-string-allowed nil))
                            ((names lambda-lists documentations declarations* bodies)
                             (parse-function-like-bindings
                              ',name definitions #',binding-parser)))
                   ;; Components in reverse evaluation-order.
                   (component :body               forms)
                   (component :declarations       declarations)
                   (component :bodies             bodies)
                   (component :local-declarations declarations*)
                   (component :documentations     documentations)
                   (component :lambda-lists       lambda-lists)
                   (component :names              names)))
                (:unparser
                 `(,(mapcar #'unparse-flet-binding
                            (component :names)
                            (component :lambda-lists)
                            (component :documentations)
                            (component :local-declarations)
                            (component :bodies))
                   ,@(component :declarations)
                   ,@(component :body))))))

  (define-flet-special-operator macrolet parse-flet-binding
    #!+sb-doc
    "MACROLET ({(name lambda-list declaration* form*)}*) body-form*

Evaluate the BODY-FORMS in an environment with the specified local
macros defined. NAME is the local macro name, LAMBDA-LIST is a
DEFMACRO style destructuring lambda list, and the FORMS evaluate to
the expansion.")

  (define-flet-special-operator flet parse-flet-binding
    #!+sb-doc
    "FLET ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings
do not enclose the definitions; any use of NAME in the FORMS will
refer to the lexically apparent function definition in the enclosing
environment.")

  (define-flet-special-operator labels parse-flet-binding
    #!+sb-doc
    "LABELS ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings
enclose the new definitions, so the defined functions can call
themselves or each other."))

;;;; [TRULY-]THE

;;; Assert that FORM evaluates to VALUE-TYPE (which may be a VALUES
;;; type). VALUE-TYPE may be a type specifier or (as a hack) a CTYPE.
;;;
;;; TRULY-THE is like the THE special form, except that it believes
;;; whatever you tell it. It will never generate a type check, but
;;; will cause a warning if the compiler can prove the assertion is
;;; wrong.
(macrolet ((define-the-special-operator (name &optional documentation)
             `(define-special-operator ,name (value-type form)
                ((:value-type 1 :evaluated nil) ; TODO :type (or type-specifier ctype)
                 (:form       1))
                ,@(when documentation
                    `((:documentation ,documentation))))))

  (define-the-special-operator the
    #!+sb-doc
    "THE value-type form

Specifies that the values returned by FORM conform to the VALUE-TYPE.

CLHS specifies that the consequences are undefined if any result is
not of the declared type, but SBCL treats declarations as assertions
as long as SAFETY is at least 2, in which case incorrect type
information will result in a runtime type-error instead of leading to
eg. heap corruption. This is however expressly non-portable: use
CHECK-TYPE instead of THE to catch type-errors at runtime. THE is best
considered an optimization tool to inform the compiler about types it
is unable to derive from other declared types.")

  (define-the-special-operator truly-the
    #!+sb-doc
    "TRULY-THE value-type form

Specifies that the values returned by FORM conform to the VALUE-TYPE,
and causes the compiler to trust this information unconditionally.

Consequences are undefined if any result is not of the declared type
-- typical symptoms including memory corruptions. Use with great
care."))

;; For the benefit of code-walkers we also add a
;; macro-expansion. (Using INFO directly to get around safeguards for
;; adding a macro-expansion for special operator.) Because :FUNCTION
;; :KIND remains :SPECIAL-FORM, the compiler never uses the macro --
;; but manually calling its MACRO-FUNCTION or MACROEXPANDing
;; TRULY-THE forms does.
#-sb-xc-host
(setf (info :function :macro-function 'truly-the)
      (lambda (whole env)
        (declare (ignore env))
        `(the ,@(cdr whole))))

;;;; SETQ

(define-special-operator setq (&whole form &rest names-and-value-forms)
  ((:names       t :evaluated nil :type symbol)
   (:value-forms t))
  #!+sb-doc
  (:documentation
   "SETQ {var form}*

Assign the value of each FORM to the variable name by the preceding
VAR.")
  (:parser
   (unless (evenp (length names-and-value-forms))
     (compiler-error "~@<Odd number of arguments to ~S: ~S.~@:>"
                     'setq form))
   (collect ((names) (value-forms))
     (loop :for (name value-form) :on names-and-value-forms :by #'cddr :do
        (names (check-variable-name name))
        (value-forms value-form))
     (component :names       (names))
     (component :value-forms (value-forms))))
  (:unparser
   (mapcan #'list (component :names) (component :value-forms))))

;;;; THROW, CATCH and UNWIND-PROTECT

(define-special-operator throw (tag result-form)
  ((:tag         1)
   (:result-form 1))
  #!+sb-doc
  (:documentation
   "THROW tag result-form

Do a non-local exit, return the values of RESULT-FORM from the CATCH
whose tag is EQ to TAG."))

;;; This is a special special form used to instantiate a cleanup as
;;; the current cleanup within the body. KIND is the kind of cleanup
;;; to make, and MESS-UP is a form that does the mess-up action. We
;;; make the MESS-UP be the USE of the MESS-UP form's continuation,
;;; and introduce the cleanup into the lexical environment. We
;;; back-patch the ENTRY-CLEANUP for the current cleanup to be the new
;;; cleanup, since this inner cleanup is the interesting one.
(define-special-operator %within-cleanup (kind mess-up &rest body)
  ((:kind    1 :evaluated nil) ; TODO :type (member ?)
   (:mess-up 1)
   (:body    t)))

;;; This is a special special form that makes an "escape function"
;;; which returns unknown values from named block. We convert the
;;; function, set its kind to :ESCAPE, and then reference it. The
;;; :ESCAPE kind indicates that this function's purpose is to
;;; represent a non-local control transfer, and that it might not
;;; actually have to be compiled.
;;;
;;; Note that environment analysis replaces references to escape
;;; functions with references to the corresponding NLX-INFO structure.
(define-special-operator %escape-fun (tag)
  ((:tag 1 :evaluated nil :type symbol))) ; tag has same type as block name

;;; Yet another special special form. This one looks up a local
;;; function and smashes it to a :CLEANUP function, as well as
;;; referencing it.
(define-special-operator %cleanup-fun (name)
  ((:name 1 :evaluated nil :type sb!impl::function-name)))

(define-special-operator catch (tag &rest body)
  ((:tag  1)
   (:body t))
  #!+sb-doc
  (:documentation
   "CATCH tag form*

Evaluate TAG and instantiate it as a catcher while the body forms are
evaluated in an implicit PROGN. If a THROW is done to TAG within the
dynamic scope of the body, then control will be transferred to the end
of the body and the thrown values will be returned."))

(define-special-operator unwind-protect (protected &rest cleanup)
  ((:protected 1)
   (:cleanup   t))
  #!+sb-doc
  (:documentation
   "UNWIND-PROTECT protected cleanup*

Evaluate the form PROTECTED, returning its values. The CLEANUP forms
are evaluated whenever the dynamic scope of the PROTECTED form is
exited (either due to normal completion or a non-local exit such as
THROW)."))

;;;; multiple-value stuff

(define-special-operator multiple-value-call (function-form &rest args)
  ((:function-form 1)
   (:args          t))
  #!+sb-doc
  (:documentation
   "MULTIPLE-VALUE-CALL function-form values-form*

Call FUNCTION-FORM, passing all the values of each VALUES-FORM as
arguments, values from the first VALUES-FORM making up the first
argument, etc."))

(define-special-operator multiple-value-prog1 (values-form &rest forms)
  ((:values-form 1)
   (:forms       t))
  #!+sb-doc
  (:documentation
   "MULTIPLE-VALUE-PROG1 values-form form*

Evaluate VALUES-FORM and then the FORMS, but return all the values of
VALUES-FORM."))

;;;; Pseudo-special operators and components for leafs, macros, lambda
;;;; applications and named applications.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (leaf-info
               (:include operator-info)
               (:constructor nil)
               (:copier nil)))

  (defstruct (variable-info
               (:include leaf-info)
               (:constructor make-variable-info (&rest components))
               (:copier nil))))

(defglobal +variable-access+
    (make-operator-component :access 1 nil))

(defglobal +variable+ (make-variable-info +variable-access+)) ; TODO where

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (self-evaluating-info
               (:include leaf-info)
               (:constructor make-self-evaluating-info ())
               (:copier nil))))

(defglobal +self-evaluating+ (make-self-evaluating-info))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (%leaf-info
               (:include leaf-info)
               (:constructor make-%leaf-info ())
               (:copier nil))))

(defglobal +leaf+ (make-%leaf-info))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (application-info
               (:include operator-info)
               (:constructor nil)
               (:copier nil))))

;;; List of (evaluated) arguments for all applications.
(defglobal +application-arguments+
    (make-operator-component :arguments t t))

;;; In the application of a LAMBDA form (i.e. ((LAMBDA ...) ...) or
;;; the application of a lexical function, the lambda-list of the
;;; lambda form or lexical function.
(defglobal +application-lambda-list+ ; TODO define a constant instead?
    (make-operator-component :lambda-list 1 nil))

;;; In the application of a LAMBDA form (i.e. ((LAMBDA ...) ...) or
;;; the application of a lexical function, the list of declarations in
;;; the body of the function.
(defglobal +application-declarations+
    (make-operator-component :declarations t nil))

;;; In the application of a LAMBDA form (i.e. ((LAMBDA ...) ...) or
;;; the application of a lexical function, the body of the lambda form
;;; or lexical function (not including declarations).
(defglobal +lambda-body+
    (make-operator-component :body t t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (lambda-application-info
               (:include application-info)
               (:constructor make-lambda-application-info
                             (&rest components))
               (:copier nil))))

(defglobal +lambda-application+
    (make-lambda-application-info
     +application-lambda-list+
     +lambda-body+
     +application-arguments+))

;;; In the application of a lexical function, the documentation in the
;;; body of the function.
(defglobal +lexical-function-documentation+
    (make-operator-component :documentation 1 nil
                             :type '(or null string)))

(defglobal +lexical-function-body+
    (make-operator-component :body 1 nil))

;;; In the application of a named function, where was the function
;;; defined? One of
;;; :GLOBAL  - TODO
;;; :LEXICAL -
;;; NIL      - No function definition for the specified name
(defglobal +function-where+
    (make-operator-component :where 1 nil
                             :type '(member nil :lexical :global)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (named-application-info
               (:include application-info)
               (:constructor make-named-application-info
                             (&rest components))
               (:copier nil))))

(defglobal +named-application+
    (make-named-application-info
     +function-where+
     +application-lambda-list+
     +lexical-function-documentation+
     +application-declarations+
     +lexical-function-body+
     +application-arguments+))

;;;
(defglobal +macro-expander+
    (make-operator-component :expander 1 nil
                             :type 'function))

(defglobal +macro-arguments+
    (make-operator-component :arguments t nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (macroid-info
               (:include operator-info)
               (:constructor nil)
               (:copier nil)))

  (defstruct (macro-info
               (:include macroid-info)
               (:constructor make-macro-info (&rest components))
               (:copier nil))))

(defglobal +macro+
    (make-macro-info +macro-expander+ +macro-arguments+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (compiler-macro-info
               (:include macroid-info)
               (:constructor make-compiler-macro-info (&rest components))
               (:copier nil))))

(defglobal +compiler-macro+
    (make-compiler-macro-info +macro-expander+ +macro-arguments+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (symbol-macro-info
               (:include macroid-info)
               (:constructor make-symbol-macro-info (&rest components))
               (:copier nil))))

(defglobal +symbol-macro+
    (make-symbol-macro-info +macro-expander+)) ; TODO where?

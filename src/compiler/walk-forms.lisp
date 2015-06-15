;;;; Special-operator-, macro- and application-aware code walker

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; TODO This file only contains original code. Still derived from CMU note?

;;;; The code in this file is organized into multiple layers
;;;; implemented on top of each other:
;;;;
;;;; 1. The fundamental layer is WALK-FORMS which only knows about
;;;;    special forms (via SPECIAL-OPERATOR-INFO) and relies on
;;;;    functions it receives as arguments for function/macro lookup
;;;;    and macro expansion.
;;;;
;;;;    WALK-FORMS is also not tied to a specific result
;;;;    representation: the result is constructed from the values
;;;;    return by the callback function. TODO better name than callback
;;;;
;;;; 2. WALK-FORMS/RECONSTITUTE TODO
;;;;
;;;; 3. Environment Interaction TODO
;;;;
;;;; 4. WALK-FORMS can call "secondary" functions in addition to the
;;;;    primary callback function (TODO better name again) for each
;;;;    walked form. These functions, or "hooks", can process or
;;;;    change the walked forms and control to which sub-forms they
;;;;    should be recursively applied. Hooks can be composed freely
;;;;    and "see through" macro expansion, if desired.
;;;;
;;;;    Hooks can be used to transparent add things like
;;;;    instrumentation to a primary code walking process such as ir1
;;;;    conversion.
;;;;
;;;;    TODO the function is ADD-HOOK-PROCESSING

(cl:in-package "SB!C")

;;;; WALK-FORMS function

;;; Helper functions
(defun %cannot-recurse (&key components function)
  (declare (ignore function))
  (compiler-error 'invalid-component-error
                  :info      :TODO
                  :component (first components)))

(defun %nop-recurse (&key components function)
  (declare (ignore function))
  (when components
    (%cannot-recurse :components components)))

(defmacro form-case (form &body clauses)
  (with-unique-names (form-var)
    (let (seen)
      (labels ((make-compound-form-test (head)
                 `(and (typep ,form-var '(cons ,head))
                       (proper-list-p ,form-var))) ; TODO correct? also for macros?
               (process-clause (clause)
                 (destructuring-bind (kind &rest body) clause
                   (when (member kind seen)
                     (compiler-error "~@<Multiple ~S clauses~@:>" kind))
                   (push kind seen)
                   (flet ((make-body (body)
                            `((return-from nil (progn ,@body)))))
                     (ecase kind
                       ;; CLHS 3.1.2.1.1 Symbols as Forms
                       (:variable
                        `(when (sb!impl::legal-variable-name-p ,form-var)
                           ,@(make-body body)))
                       ;; SBCL extension
                       (:leaf
                        `(when (leaf-p ,form-var)
                           ,@(make-body body)))
                       ;; CLHS 3.1.2.1.3 Self-Evaluating Objects
                       (:self-evaluating
                        `(when (atom ,form-var)
                           ,@(make-body body)))
                       ;; CLHS 3.1.2.1.2 Conses as Forms
                       ;; CLHS 3.1.2.1.2.1 Special Forms
                       (:special-form
                        ;; TODO use SPECIAL-OPERATOR-P when possible?
                        `(when ,(make-compound-form-test 'symbol)
                           (let ((,(first body) (find-special-operator-info
                                                 (first ,form-var))))
                             (when ,(first body)
                               ,@(make-body (rest body))))))
                       ;; CLHS 3.1.2.1.2.4 Lambda Forms
                       (:lambda-application
                        `(when ,(make-compound-form-test '(cons (eql lambda)))
                           ,@(make-body body)))
                       ;; CLHS 3.1.2.1.2.2 Macro Forms
                       ;; CLHS 3.1.2.1.2.3 Function Forms
                       (:named-application
                        `(when ,(make-compound-form-test 'symbol)
                           ,@(make-body body)))
                       ((t otherwise)
                        `(progn ,@body)))))))
        `(let ((,form-var ,form))
           (block nil
             ,@(mapcar #'process-clause clauses)))))))

(defvar *variable-access* :read)
(declaim (type (member :read :write :bind) *variable-access*))

;;; TODO (declaim (ftype ) walk-forms)
;;; From src/pcl/walk.lisp (TODO proper citation):
;;;
;;;   "It is recommended that a program-analyzing-program process a form
;;;    that is a list whose car is a symbol as follows:
;;;
;;;     1. If the program has particular knowledge about the symbol,
;;;        process the form using special-purpose code. All of the
;;;        standard special forms should fall into this category.
;;;     2. Otherwise, if MACRO-FUNCTION is true of the symbol apply
;;;        either MACROEXPAND or MACROEXPAND-1 and start over.
;;;     3. Otherwise, assume it is a function call. "
(defun walk-forms (result-type function form
                   classify-variable classify-application expand-macro)
  #!+sb-doc
  "Apply FUNCTION to FORM and its sub-forms, return a RESULT-TYPE sequence.

The lambda-list of FUNCTION has to be compatible with

  (instead recurse current-form kind name &rest components)

where (in the following, FUNCTION*, FORM*, NAME* and COMPONENTS* refer
to the above parameters where ambiguity would otherwise arise)

  INSTEAD
    A function with lambda-list (&key function form) that FUNCTION*
    can call to walk FORM with FUNCTION independently of the current
    FUNCTION* and FORM*.

    Default values of the FUNCTION and FORM keyword parameters are
    FUNCTION* and FUNCTION*'s CURRENT-FORM argument respectively.

  RECURSE
    A function with lambda-list (&key function components) that
    FUNCTION* can call to walk COMPONENTS of FORM* with FUNCTION. If
    supplied, COMPONENTS has to be a plist of the form

      (COMPONENT-NAME1 SUB-FORM1 COMPONENT-NAME2 SUB-FORM2 ...)

    where COMPONENT-NAMEN are the keywords naming components according
    to KIND and NAME. Note that only evaluated components can be
    specified for recursive walking.

    Default values of the FUNCTION and COMPONENTS keyword parameters
    are FUNCTION* and the evaluated COMPONENTS* FUNCTION* received
    respectively.

  KIND
    An object of one of the following types (see CLHS 3.1.2.1 \"Form
    Evaluation\" for the specification of forms in Common Lisp):

    SB-C:LEAF-INFO subtypes:

      SB-C:VARIABLE-INFO
        FORM is a reference to a variable. NAME is identical to
        FORM. Depending on the function supplied as CLASSIFY-VARIABLE,
        the following COMPONENTS may be available:

          :TYPE     TYPE-SPECIFIER
          :CONSTANT BOOLEAN
          :WHERE    :GLOBAL | :LEXICAL | NIL
          :ACCESS   :READ | :WRITE | :BIND

      :SELF-EVALUATING | :LEAF (SBCL extension)
        FORM is a self-evaluating object or (compiler-internal) LEAF
        structure. NAME is identical to FORM.

    SB-C:SPECIAL-OPE
      FORM is a special form whose CAR is the special operator
      NAME. COMPONENTS (see below) contains the different syntactic
      components of the special form and depends on NAME.

      For example, the THROW special operator is described as

        KIND       = :SPECIAL-FORM
        NAME       = 'THROW
        COMPONENTS = :TAG TAG-FORM :RESULT-FORM RESULT-FORM

    SB-C:APPLICATION-INFO subtypes:

      SB-C:LAMBDA-APPLICATION-INFO | SB-C:NAMED-APPLICATION-INFO
        FORM is one of the two applications

          (FUNCTION-NAME             ARGUMENTS)
          ((LAMBDA LAMBDA-LIST BODY) ARGUMENTS)

        NAME is FUNCTION-NAME in the former case and the symbol
        CL:LAMBDA in the latter case. Depending on the function
        supplied as CLASSIFY-APPLICATION, the following COMPONENTS may
        be available:

          :ARGUMENTS   ARGUMENT-FORMS            both cases
          :WHERE       :GLOBAL | :LEXICAL | NIL  only named case
          :LAMBDA-LIST LAMBDA-LIST               only lambda case
          :BODY        BODY                      only lambda case

    SB-C:MACROID-INFO subtypes:

      SB-C:MACRO-INFO | SB-C:COMPILER-MACRO-INFO | SB-C:SYMBOL-MACRO-INFO
        FORM is a macro application of the form

          (NAME ARGUMENTS)

        NAME is NAME*, the name of macro, function or
        symbol-macro. Depending on the functions supplied as
        CLASSIFY-VARIABLE and CLASSIFY-APPLICATION, the following
        COMPONENTS may be available:

          :ARGUMENTS ARGUMENTS
          :WHERE     :GLOBAL | :LEXICAL
          :EXPANDER  FUNCTION

        For all kinds of macros, calling RECURSE expands the macro (by
        calling the function supplied via EXPAND-MACRO with the value
        of the :EXPANDER component) and walks the expansion.

      SB-C:SETQ-WITH-SYMBOL-MACROS-INFO
        FORM is a SETQ-assignment in which at least one of the
        assigned-to variables is a symbol macro.

        The following components are available:

          :ARGUMENTS       ARGUMENTS
          :NAMES           SYMBOLS
          :VALUE-FORMS     FORMS
          :SYMBOL-MACRO-PS GENERALIZED-BOOLEANS
          :EXPANDER        FUNCTION

        Calling RECURSE reconstitutes FORM by replacing SETQ with SETF
        and expanding all symbol macros with their respective
        expansions.

  NAME
    Depends on KIND. See description of KIND for possible meanings.

  COMPONENTS
    A plist of component names and component forms depending on KIND
    and NAME. Individual components can be received by using &KEY
    parameters in FUNCTION's lambda-list. See description of KIND for
    possible values.

FUNCTION's return value is incorporated as an element in nested
sequences of type RESULT-TYPE that are generally shaped like FORM and
the tree of its sub-forms. However, transformations performed by
FUNCTION can of course lead to different shapes.

CLASSIFY-VARIABLE has to be a function with lambda-list (form) that,
when called with symbol, returns up to four values:
1) a kind which has to be either :VARIABLE or :SYMBOL-MACRO
2) a scope which has to be either :GLOBAL or :LEXICAL
3) a type specifier describing the type of the variable
4) a plist TODO
When FORM does not name a variable or symbol macro, kind has to be
NIL (or no values can be returned).

CLASSIFY-APPLICATION has to be a function with lambda-list (form)
that, when called with a function or macro application form (but not a
special form), returns up to three values:
1) a kind which has to be either :APPLICATION, :COMPILER-MACRO
   or :MACRO
2) a scope which has to be either :GLOBAL or :LEXICAL
3) a plist TODO
When the CAR of FORM does not name a function or macro, kind has to be
NIL (or no values can be returned).

EXPAND-MACRO TODO

Example:

  (sb-c:walk-forms
   'list (lambda (instead recurse form kind name &rest components
                  &key value-forms &allow-other-keys)
           (declare (ignore instead))
           (if (and (typep kind sb-c:special-operator-info) (eq name 'setq)) ; TODO update
               (let ((value-forms
                      (substitute
                       2 1 (second (funcall recurse
                                            :components (list :value-forms
                                                              value-forms))))))
                 (apply #'sb-c:reconstitute form kind name
                        (list* :value-forms value-forms components)))
               (apply #'sb-c:reconstitute form kind name
                      (append (funcall recurse) components))))
   '(let (a) (setq a 1))
   nil nil nil) ; don't actually supply NIL here
  => (LET (A) (SETQ A 2))

TODO explain"
  (let ((function (coerce function 'function)))
    (labels ((make-instead (function form) ; TODO check what can be dxified
               (named-lambda instead (&key (function function) (form form))
                 (rec/restart function form)))
             ;; Process an application or a special form. TODO explain more
             (process-application-like (function form info name &rest components)
               (declare (type (or special-operator-info application-info) info))
               (labels ((find-info (name)
                          (or (find name (operator-info-components info)
                                    :key #'operator-component-name)
                              (error 'invalid-component-error
                                     :info      info
                                     :component name)))
                        (recurse/component (function form component)
                          (declare (type operator-component component))
                          (aver (operator-component-evaluated component))
                          (let ((*variable-access*
                                 (if (not (typep component 'operator-access-component))
                                     *variable-access*
                                     (operator-access-component-access component))))
                            (ecase (operator-component-cardinality component)
                              ((? 1)
                               (rec/restart function form))
                              ((t)
                               (dx-flet ((one-form (form)
                                           (rec/restart function form)))
                                 (map result-type #'one-form form))))))
                        (recurse/application-like (&key
                                                   (components components)
                                                   (function function))
                          (loop :for (name form) :on components :by #'cddr
                             :for info = (find-info name)
                             :when (operator-component-evaluated info)
                             :collect name
                             :and :collect (recurse/component function form info))))
                 (apply function
                        (make-instead function form) #'recurse/application-like
                        form info name components)))
             (process-application-like/setq
                 (function form info name &rest components)
               (declare (type special-operator-info info))
               (binding* (((names value-forms)
                           (parse-setq-contents form (rest form) #'identity))
                          (symbol-macro-ps ; are some variables symbol-macros?
                           (mapcar (lambda (name)
                                     (maybe-symbol-macro-info
                                      classify-variable name))
                                   names)))
                 (if (every #'null symbol-macro-ps)
                     (apply #'process-application-like
                            function form info name components)
                     (apply #'process-macro
                            function form +setq-with-symbol-macros+
                            (lambda (form*)
                              (aver (eq form form*))
                              (expand-setq+symbol-macro
                               names value-forms symbol-macro-ps expand-macro))
                            (list* :symbol-macro-ps symbol-macro-ps
                                   components)))))
             (process-application-like/maybe-setq
                 (function form info name &rest components)
               (declare (type (or special-operator-info application-info) info))
               (apply (case (first form)
                        (setq #'process-application-like/setq)
                        (t    #'process-application-like))
                      function form info name components))

             ;; TODO describe
             (process-macro (function form info expander &rest components)
               (declare (type macroid-info info))
               (multiple-value-bind (name arguments argumentsp)
                   (if (listp form)
                       (values (first form) (rest form) t)
                       form)
                 (labels ((recurse/macro (&key (function function))
                            (let ((expansion (funcall expand-macro
                                                      info expander form)))
                              (rec/restart function expansion))))
                   (apply function
                          (make-instead function form)
                          #'recurse/macro
                          form info name
                          :expander expander
                          (append
                           (when argumentsp
                             (list :arguments arguments))
                           components)))))
             ;; Note: "leaf" as in "tree node without children", not
             ;; only SB-C:LEAF.
             (process-leaf (function form info &rest components)
               (apply function
                      (make-instead function form) #'%nop-recurse
                      form info form components))
             (rec (function form)
               (form-case form
                 ;; Various "leaf" nodes => supply %NOP-RECURSE via
                 ;; PROCESS-LEAF.
                 (:variable
                  (multiple-value-bind (kind where type plist)
                      (funcall classify-variable form)
                    (ecase kind
                      (:variable
                       (apply #'process-leaf function form +variable+
                              :where  where
                              :access *variable-access*
                              (append (when type (list :type type))
                                      plist)))
                      (:symbol-macro
                       (let ((plist (copy-list plist))
                             (expansion (getf plist :expansion)))
                         (remf plist :expansion)
                         (apply #'process-macro function form +symbol-macro+
                                (lambda (form)
                                  (declare (ignore form))
                                  expansion)
                                :where where
                                plist))))))
                 (:leaf
                  (process-leaf function form +leaf+))
                 (:self-evaluating
                  (process-leaf function form +self-evaluating+))

                 ;; Special operator => use the associated parser and
                 ;; info.
                 (:special-form info
                  (let ((name (special-operator-info-name info)))
                    (with-parsed-special-operator (form name &rest components) ; TODO add hyperspec reference if parse fails
                      (apply #'process-application-like/maybe-setq
                             function form info name components))))

                 ;; ((lambda ...) ...) => application with lambda-list
                 ;; and body as extra information.
                 (:lambda-application
                  (with-parsed-special-operator (`(function ,(first form)) 'function
                                                 &rest components)
                    (apply #'process-application-like
                           function form +lambda-application+ 'lambda
                           :arguments (rest form)
                           components)))

                 ;; Application of named things: [compiler-]macros and
                 ;; functions.
                 (:named-application
                  (multiple-value-bind (kind where plist)
                      (funcall classify-application form)
                    (case kind
                      ((:compiler-macro :macro)
                       (let ((plist (copy-list plist))
                             (expander (getf plist :expander)))
                         (remf plist :expander)
                         (apply #'process-macro function form
                                (ecase kind
                                  (:macro          +macro+)
                                  (:compiler-macro +compiler-macro+))
                                expander
                                :where where
                                plist)))
                      (:application
                       (apply #'process-application-like
                              function form +named-application+ (first form)
                              :where     where
                              :arguments (rest form)
                              plist))
                      (t
                       (bug "invalid named application"))))) ; TODO proper error? can this happen? yes, if the caller supplies an incorrect CLASSIFY-APPLICATION

                 ;;
                 (t
                  (compiler-error 'invalid-form-error :form form))))
             (rec/restart (function form)
               (restart-case
                   (rec function form)
                 (use-value (value &optional condition)
                   :report (lambda (stream)
                             (format stream "~@<Process a different ~
                                             form instead of ~S.~@:>"
                                     form))
                   (declare (ignore condition))
                   (rec/restart function value)))))
      (rec/restart function form))))

;;;; Reconstituting variant

(defvar *recons-print* nil) ; TODO temp

(defun recons (cons car cdr)
  (if (and (eq (car cons) car)
           (eq (cdr cons) cdr))
      (if *recons-print*
          (second (print (list :reuse cons)))
          cons)
      (if *recons-print*
          (second (print (list :fresh (cons car cdr))))
          (cons car cdr))))

(defun relist (list &rest objects)
  (if (null objects)
      '()
      (%relist list objects nil)))

(defun relist* (list &rest objects)
  (%relist list objects t))

(defun %relist (list objects *p)
  (labels ((rec (list objects)
             (cond
               ((not (null (cdr objects)))
                (recons list (car objects) (rec (cdr list) (cdr objects))))
               (*p
                (car objects))
               (t
                (recons list (car objects) nil)))))
    (declare (dynamic-extent #'rec))
    (rec list objects)))

(defun reconstitute (form kind name &rest components)
  #!+sb-doc
  "Reconstitute FORM by unparsing KIND, NAME and COMPONENTS.

FORM and the unparsed representation share structure where possible."
  (etypecase kind
    ((or leaf-info symbol-macro-info)
     name)
    (special-operator-info
     (let ((info (or (find-special-operator-info name)
                     (error (!uncross-format-control
                             "~@<~/sb!impl:print-symbol-with-prefix/ ~
                              does not name a special operator.~@:>")
                            name))))
       (apply #'relist form (apply (special-operator-info-unparser info)
                                   components)))) ; TODO how to RELIST COMPONENTS?
    ;; Either application of named function or ((lambda ...) ...).
    (application-info
     (destructuring-bind (&key lambda-list body arguments &allow-other-keys)
         components
       (let ((arguments (apply #'relist (rest form) arguments)))
         (cond
           ((neq name 'lambda)
            (relist* form name arguments))
           (t
            (relist* form (list* 'lambda lambda-list body)
                     arguments))))))
    (macroid-info
     (destructuring-bind (&key arguments &allow-other-keys) components
       (relist* form name (apply #'relist (rest form) arguments))))))

(defun walk-forms/reconstitute
    (function form
     classify-variable classify-application expand-macro)
  #!+sb-doc
  "Like WALK-FORMS but provides FUNCTION with a way to reconstitute forms.

Thus FUNCTION's lambda-list changes (compared to WALK-FORMS) to

  (instead recurse reconstitute current-form kind name &rest components)

where RECONSTITUTE is a function with lambda-list

  (&optional form kind name &rest components)

which reconstitutes FORM, potentially with arbitrary modifications, by
unparsing KIND, NAME and COMPONENTS. FORM and the unparsed
representation share structure where possible.

FORM, KIND, NAME and COMPONENTS default to CURRENT-FORM, KIND*, NAME*
and COMPONENTS* respectively."
  (dx-flet ((do-form (instead recurse form kind name &rest components)
              (dx-flet ((reconstitute* (&optional (form form) (kind kind) (name name)
                                        &rest components*)
                          (apply #'reconstitute form kind name
                                 (append components* components))))
                (apply function instead recurse #'reconstitute*
                       form kind name components))))
    (walk-forms 'list #'do-form form
                classify-variable classify-application expand-macro)))

;;;; Environment interaction

;;; Variable and symbol-macro lookup

(defun classify-variable-form/lexical (form env)
  (let* ((*lexenv* env)
         (variable (lexenv-find form vars)))
    (typecase variable
      (leaf
       (when (lambda-var-p variable)
         (values :variable :lexical)))
      ((cons (eql variable))
       (values :variable :lexical (second variable) (third variable)))
      ((cons (eql macro))
       (values :symbol-macro :lexical nil
               (list :expansion (cdr variable))))
      (heap-alien-info
       (values :variable :lexical :alien)))))

(defun classify-variable-form/global (form)
  (let* ((kind (info :variable :kind form))
         (type (info :variable :type form))
         (type (when type
                 (type-specifier type))))
    #+TODO (when (and (eq kind :unknown) (not (check-deprecated-variable form)))
      )
    (ecase kind
      (:global
       (values :variable :global type))
      (:special
       (values :variable :global type '(:special t))) ; TODO put type into plist?
      (:constant
       (values :variable :global type '(:constant t)))
      (:macro
       (let ((expansion (info :variable :macro-expansion form)))
         (values :symbol-macro :global type
                 (list :expansion expansion))))
      (:alien
       (values :variable :global :alien))
      (:unknown
       (values :variable)))))

(defun classify-variable-form (form env)
  #!+sb-doc
  "TODO"
  (flet ((maybe-return (&optional kind where type plist)
           (when kind
             (return-from classify-variable-form
               (values kind where type plist)))))
    (multiple-value-call #'maybe-return
      (classify-variable-form/lexical form env))
    (multiple-value-call #'maybe-return
      (classify-variable-form/global form))))

(defun maybe-symbol-macro-info (classify-variable name)
  (multiple-value-bind (kind where type plist)
      (funcall classify-variable name)
    (when (eq kind :symbol-macro)
      (list kind where type plist)))) ; TODO make-symbol-macro-info

(defun note-lexical-variable (name env &rest plist &key &allow-other-keys)
  #!+sb-doc
  "TODO"
  (push `(,name . (variable nil ,plist)) (lexenv-vars env)))

;;; Function and (compiler-)macro lookup

(defun classify-application-form/lexical (form env)
  (let* ((name       (first form))
         (definition (if (leaf-p name)
                         name
                         (let ((*lexenv* env))
                           (lexenv-find name funs)))))
    (typecase definition ; TODO etypecase
      (global-var ; TODO global-var indicates :global function?
       (values :application :lexical (list :definition definition)))
      (functional
       (values :application :lexical (list :definition definition))) ; TODO cannot work, will be opaque
      ((cons (eql lambda)) ; TODO only used here, not in the rest of the compiler
       (values :application :lexical (cdr definition)))
      ((cons (eql macro))
       (values :macro :lexical (list :expander (cdr definition)))))))

(defun classify-application-form/global (form)
  (let* ((name (first form))
         (kind (info :function :kind name)))
    (ecase kind
      (:function
       (values :application :global (list :definition (find-free-fun name "shouldn't happen! (no-cmacro)")))) ; TODO
      (:macro
       (let ((expander (info :function :macro-function name)))
         (values kind :global (list :expander expander))))
      ((nil)
       (values :application nil))))) ; unknown function

(defun classify-application-form (form env)
  #!+sb-doc
  "TODO"
  (flet ((maybe-return (&optional kind where plist)
           (when kind
             (return-from classify-application-form
               (values kind where plist)))))
    ;; Neither lexical nor global
    (multiple-value-bind (macro-function function-name) ; TODO do we have worry about whether FORM is (funcall ...)? see expand-compiler-macro
        (let ((*lexenv* env))
          (find-compiler-macro (first form) form))
      ;; CLHS 3.2.2.1.3 specifies that NOTINLINE suppresses
      ;; compiler-macros.
      (when (and macro-function
                 (not (fun-lexically-notinline-p function-name)))
        (maybe-return :compiler-macro :global
                      (list :expander macro-function))))
    (multiple-value-call #'maybe-return
      (classify-application-form/lexical form env))
    (multiple-value-call #'maybe-return
      (classify-application-form/global form))))

(defun note-lexical-function (name lambda-list documentation declarations body
                              env)
  #!+sb-doc
  "TODO"
  (push `(,name . (lambda . ,(list :lambda-list   lambda-list
                                   :documentation documentation
                                   :declarations  declarations
                                   :body          body)))
        (lexenv-funs env)))

;;; Macro handling

(defun expand-macro (kind expander form env)
  #!+sb-doc
  "TODO"
  (declare (type macroid-info)
           (type function expander))
  (etypecase kind
    (macro-info
     (let ((*lexenv* env))
       (careful-expand-macro expander form)))
    (compiler-macro-info
     (let ((*lexenv* env))
       (careful-expand-macro expander form t)))
    ((or symbol-macro-info setq-with-symbol-macros-info)
     (funcall expander form))))

(defun expand-setq+symbol-macro (names value-forms symbol-macro-ps
                                 expand-macro)
  (dx-flet ((expand-symbol (name value-form symbol-macro-p) ; TODO store SYMBOL-MACRO-INFOs in symbol-macro-p?
              (destructuring-bind (&optional kind where type plist)
                  symbol-macro-p
                (declare (ignore where type))
                (list (if kind
                          (funcall expand-macro
                                   +symbol-macro+
                                   (lambda (form)
                                     (declare (ignore form))
                                     (getf plist :expansion))
                                   name)
                          name)
                      value-form))))
    `(setf ,@(mapcan #'expand-symbol names value-forms symbol-macro-ps))))

(defun note-lexical-macro (name lambda-list declarations body
                           env compile-env instead)
  #!+sb-doc
  "TODO"
  (with-unique-names (whole environment)
    (let* ((body (parse-defmacro
                  lambda-list whole (list body) name 'macrolet
                  :environment environment))
           (form (funcall instead
                          :form `(lambda (,whole ,environment)
                                   ,@declarations
                                   ,body)))
           (expander (compile-in-lexenv
                      nil (second form) ; remove (FUNCTION ...)
                      (make-restricted-lexenv compile-env))))
      (push `(,name . (macro . ,expander)) (lexenv-funs env)))))

(defun note-lexical-symbol-macro (name expansion env)
  #!+sb-doc
  "TODO"
  (push `(,name . (macro . ,expansion)) (lexenv-vars env)))

;;;; Hooks TODO steal some sentences from file header?
;;;;
;;;; This code is partially based on the "composable codewalking" work
;;;; by Paul Khuong.
;;;;
;;;; The goal is to allow invocation of hook functions for arbitrary
;;;; forms during code walking. The main idea is to wrap forms for
;;;; which hooks should be invoked in pseudo-special forms headed by a
;;;; %WRAPPER special operator that mark the respective wrapped form
;;;; as having an attached hook and store the actual hook:
;;;;
;;;;   (%WRAPPER #<FUNCTION TRACER> (LET ...))
;;;;
;;;; Hooks can mark sub-forms of the forms they are invoked with in
;;;; the same manner.
;;;;
;;;; It should be possible to freely mix multiple hooks during a
;;;; single code walking.
;;;;
;;;; TODO macro handling
;;;;
;;;;   (walk-forms ... (add-hook-processing FUNCTION) FORM-WITH-WRAPPERS ...)

;;;; Pseudo-special operators

(define-special-operator %wrapper (hook form)
  ((:hook 1 :evaluated nil :type function)
   (:form 1))
  #!+sb-doc
  (:documentation
   "Pseudo-special operator - only used during certain code walking operations.

    Indicates that the wrapped FORM should be processed and
    potentially transformed by HOOK during code walking."))

(define-special-operator %post-macro-wrapper (hook form)
  ((:hook 1 :evaluated nil :type function)
   (:form 1))
  #!+sb-doc
  (:documentation
   "Pseudo-special operator - only used during certain code walking operations.

    Indicates that the wrapped FORM should be first MACROEXPANDed and
    then processed and potentially transformed by HOOK during code
    walking."))

;;;; Wrapper utilities

(defun %wrap (wrapper-name hook form)
  `(,wrapper-name ,(coerce hook 'function) ,form))

(defun wrap (hook form)
  (%wrap '%wrapper hook form))

(defun wrap-walked-form (wrapper-name hook form kind name &rest components)
  (declare (ignore name))
  (when (and (typep kind 'variable-info)
             (member (getf components :access) '(:write :bind)))
    (return-from wrap-walked-form form))

  (%wrap wrapper-name hook form))

(defun make-wrap-walked-form (kind name hook)
  (declare (ignore name))
  (typecase kind
    (macroid-info
     (named-lambda wrap-with-hook/%post-macro-wrapper
         (form kind name &rest components)
       (aver (not (typep form '(cons (eql %post-macro-wrapper)))))
       (apply #'wrap-walked-form '%post-macro-wrapper hook
              form kind name components)))
    (t
     (named-lambda wrap-with-hook/%wrapper
         (form kind name &rest components)
       (aver (not (typep form '(cons (eql %post-macro-wrapper)))))
       (apply #'wrap-walked-form '%wrapper hook
              form kind name components)))))

(defun strip-wrappers (instead form)
  (let ((wrappers '()))
    (labels ((do-form (instead recurse form kind name &rest components)
               (declare (ignore recurse))
               (cond
                 ;; Collect and strip all kinds of wrappers.
                 ((and (typep kind 'special-operator-info)
                       (member name '(%wrapper %post-macro-wrapper)
                               :test #'eq))
                  (let ((hook (getf components :hook))
                        (form (getf components :form)))
                    (push (list name hook) wrappers)
                    (funcall instead :function #'do-form :form form)))
                 ;; Stop at the first non-wrapper form.
                 (t
                  form))))
      (declare (dynamic-extent #'do-form))
      (values (funcall instead :function #'do-form :form form) wrappers))))

(declaim (ftype (sfunction (function) function) add-hook-processing))
(defun add-hook-processing (function)
  #!+sb-doc
  "Wrap FUNCTION in a function that executes code walking hooks.

The returned function can be used as an argument to WALK-FORMS. For a
form, WALK-FORMS will call all hooks attached to the form and then
call FUNCTION on the resulting form.

TODO signatures

TODO macros

TODO composability

Example:

  (sb-c:walk-forms
   'list
   (sb-c:add-hook-processing
    (lambda (instead recurse form kind name &rest components)
      (apply #'sb-c:reconstitute form kind name
             (append (funcall recurse) components))))
   (sb-c:wrap
    (lambda (wrap recurse form kind name &rest components)
      (if (member kind '(:special-form :constant)) ; TODO update
          `(progn ,(apply #'sb-c:reconstitute form kind name
                          (append (funcall recurse
                                   :function (lambda (i r sub-form &rest args)
                                               (declare (ignore i r args))
                                               (funcall wrap sub-form)))
                                  components)))
          form))
    '(setq a 1))
   nil nil nil) ; don't actually supply NIL here
  ; WALK-FORMS encounters (%wrapper <hook> (setq a 1)
  ;   hook transforms (setq a 1) => (progn (setq a (%wrapper <hook> 1)))
  ; WALK-FORMS encounters (progn (setq a (%wrapper <hook> 1)))
  ;   no (outermost) wrappers
  ;   primary walk function recurses into PROGN, then into SETQ,
  ;   reconstituting results without further modification
  ; WALK-FORMS encounters (%wrapper <hook> 1)
  ;   hook transforms 1 => (progn 1)
  ; WALK-FORMS encounters (progn 1)
  ;   no (outermost) wrappers
  ;   primary walk functions recurses into PROGN, then \"into\" 1,
  ;   reconstituting results without further modification
  => (PROGN (SETQ A (PROGN 1)))

In the example, the \"primary\" walk function given to WALK-FORMS just
reconstitutes results recursive processing into the same \"shape\" as
the original form without further modification. This function is
wrapper by ADD-HOOK-PROCESSING as described above. A hook doing an
actual transformation is attached to the outermost input form (it
could be attached anywhere else as well). This hooks does two things:

1. It wraps any form of kind :SPECIAL-OPERATOR or :CONSTANT in a
   PROGN via `(progn ,(RECONSTITUTE ...)).

2. (and this is the tricky/interesting part) It marks
   sub-forms (i.e. the ... above) for further processing by the
   hook. This is achieved by calling the RECURSE function to descend
   into sub-forms and calling WRAP on the visited sub-forms."
  (labels
      ((re-wrap (wrappers form &optional name)
         (reduce (lambda (form wrapper)
                   (destructuring-bind (name* hook) wrapper
                     (%wrap (or name name*) hook form)))
                 wrappers :initial-value form))
       ;; Apply WRAPPER to FORM and return the resulting form.
       (apply-wrapper (instead form wrapper)
         (destructuring-bind (name hook) wrapper
           (aver (eq name '%wrapper))
           (funcall
            instead
            :function (lambda (instead recurse form kind name &rest args)
                        (declare (ignore instead))
                        (let ((wrap (make-wrap-walked-form
                                     kind name hook)))
                          (apply hook wrap recurse form kind name args)))
            :form     form)))
       ;; Apply WRAPPERS to FORM in the correct order and return the
       ;; resulting form. Take care to hide %POST-MACRO-WRAPPER forms
       ;; inserted by inner hooks from outer hooks.
       (apply-wrappers (instead wrappers form)
         (collect ((re-wrappers))
           (let ((result
                  (reduce
                   (lambda (form wrapper)
                     (let ((result (apply-wrapper instead form wrapper)))
                       (typecase result ; hide %POST-MACRO-WRAPPERs
                         ((cons (eql %post-macro-wrapper))
                          (re-wrappers (butlast result))
                          (third result))
                         (t
                          result))))
                   wrappers :initial-value form)))
             (re-wrap (re-wrappers) result))))
       ;; macroexpand FORM via FUNCTION. Slightly tricky because we
       ;; want to ensure the macroexpansion is done through a call of
       ;; FUNCTION.
       (expand (instead form)
         (labels ((capture (instead recurse form &rest args)
                    (declare (ignore instead recurse args))
                    form)
                  (capturing-delegate (instead recurse form kind &rest args)
                    (aver (typep kind 'macroid-info))
                    (flet ((instead (&rest args &key (function function) &allow-other-keys) ; TODO experimental; add explanation if it works
                             (apply instead :function function args))
                           (recurse (&rest args)
                             (apply recurse :function #'capture args)))
                      (apply function #'instead #'recurse form kind args))))
           (funcall instead :function #'capturing-delegate :form form)))
       ;; Augmented function. Handles %WRAPPER and %POST-MACRO-WRAPPER
       ;; and dispatches to FUNCTION for everything else.
       (hook-processing (instead recurse form kind name &rest components)
         (cond
           ((and (typep kind 'special-operator-info)
                 (eq name '%wrapper))
            ;; Collect nested %WRAPPERs, apply their hooks to the
            ;; wrapped form and continue with the resulting
            ;; potentially transformed form.
            (binding* (((form wrappers) (strip-wrappers instead form))
                       (transformed (apply-wrappers instead wrappers form))) ; TODO unless (eq transformed form)?
              (funcall instead :form transformed)))
           ((and (typep kind 'special-operator-info)
                 (eq name '%post-macro-wrapper))
            ;; Collect nested %POST-MACRO-WRAPPERs, macroexpand FORM,
            ;; re-wrap the expansion in suitable %WRAPPERs.
            (binding* (((form wrappers) (strip-wrappers instead form))
                       (expansion (expand instead form))
                       (re-wrapped (re-wrap wrappers expansion '%wrapper)))
              (funcall instead :form re-wrapped)))
           (t
            ;; Delegate to original FUNCTION.
            (apply function instead recurse
                   form kind name components)))))
    #'hook-processing))

(declaim (type function *reconstitute*))
(defvar *reconstitute*)

(defun add-hook-processing/reconstitute (function)
  #!+sb-doc
  "Like ADD-HOOK-PROCESSING but for use with WALK-FORMS/RECONSTITUTE."
  (labels ((inject (instead recurse form kind name &rest components)
             (apply function instead recurse *reconstitute*
                    form kind name components)))
    (let ((hooks+inject (add-hook-processing #'inject)))
      (labels ((hook-processing (instead recurse reconstitute
                                 form kind name &rest components)
                 (let ((*reconstitute* reconstitute))
                   (apply hooks+inject instead recurse
                          form kind name components))))
        #'hook-processing))))

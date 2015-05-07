;;;; the usual place for DEFINE-IR1-TRANSLATOR forms (and their close
;;;; personal friends)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; DEFINE-IR1-TRANSLATOR and support ; TODO rename translator -> converter?

;;; Return the SPECIAL-OPERATOR-INFO instance for the special operator
;;; designated by NAME or NIL if there is no such special operator.
#+TODO (declaim (ftype (sfunction (symbol) (or null special-operator-info))
                       find-ir1-converter))
(defun find-ir1-converter (name)
  (declare (notinline info)) ; TODO why is this necessary?
  (values (info :function :ir1-converter name)))

;;; Install NEW-VALUE, a SPECIAL-OPERATOR-INFO instance for the
;;; special operator designated by NAME.
#+TODO (declaim (ftype (sfunction (function symbol) function)
                       (setf find-ir1-converter)))
(defun (setf find-ir1-converter) (new-value name)
  (declare (notinline info (setf info))) ; TODO why is this necessary?
  (multiple-value-bind (old oldp)
      (info :function :ir1-converter name)
    (when oldp
      (cerror "Replace the existing ir1 converter"
              "~@<~S already has the ir1 converter ~A~@:>"
              name old)))
  (setf (info :function :ir1-converter name) new-value))

(defmacro define-ir1-translator (name
                                 (component-vars
                                  &optional
                                  instead-var recurse-var
                                  (start-var 'unused-start)
                                  (next-var 'unused-next)
                                  (result-var 'unused-result))
                                 &body body)
  ;; Compile-time checks.
  (let* ((info       (find-special-operator-info-or-lose name))
         (components (special-operator-info-components info)))
    (dolist (component component-vars)
      (unless (find (keywordicate component) components ; TODO make a function special-operator-info-component[-or-lose]
                    :key #'operator-component-name)
        (error "~@<No component ~S in special operator ~S. Components ~
                are ~{~A~^, ~}~@:>"
               component name (mapcar #'operator-component-name components)))))

  ;; Expansion.
  (binding* ((converter-name (symbolicate "IR1-CONVERT-" name "-TODO-REMOVE-THIS"))
             ((body-forms body-declarations documentation)
              (parse-body body)))
    (with-unique-names (whole-var n-env)
      `(progn
         #+TODO-LATER (declaim (ftype (function (function function ctran ctran (or lvar null) t &key)
                                   (values))
                         ,converter-name))
         (defun ,converter-name (,instead-var ,recurse-var
                                 ,start-var ,next-var ,result-var
                                 ,whole-var &key ,@component-vars
                                 &aux (,n-env *lexenv*))
           (declare (type function ,instead-var ,recurse-var)
                    (ignorable ,instead-var ,recurse-var ,start-var ,next-var ,result-var))
           (declare (ignore ,whole-var ,n-env)) ; TODO
           ,@body-declarations
           ,@documentation
           (print (list :enter ',name))
           (dx-flet (,@(when instead-var
                         `((,instead-var (&rest args)
                             (apply ,instead-var args))))
                     ,@(when recurse-var
                         `((,recurse-var (&rest args)
                             (apply ,recurse-var args)))))
             ,@body-forms)
           (print (list :leave ',name))
           (values))
         (setf (find-ir1-converter ',name) #',converter-name)
         ',name))))

;;;; IR1 conversion of special operators
;;;;
;;;; The order of operators corresponds to
;;;; src/compiler/special-operators.lisp.


;;;; Special operators for control

(define-ir1-translator progn ((forms) instead recurse start next result)
  (ir1-convert-progn-body/new start next result instead recurse forms))

(define-ir1-translator if ((test then else)
                           instead recurse start next result)
  (let* ((test-ctran (make-ctran))
         (test-lvar (make-lvar))
         (then-ctran (make-ctran)) ; TODO (with-blocks ((then-ctran then-block) (else-ctran else-block)
         (then-block (ctran-starts-block then-ctran))
         (else-ctran (make-ctran))
         (else-block (ctran-starts-block else-ctran))
         (node (make-if :test test-lvar
                        :consequent then-block
                        :alternative else-block)))
    ;; IR1-CONVERT-MAYBE-PREDICATE requires DEST to be CIF, so the
    ;; order of the following two forms is important
    (setf (lvar-dest test-lvar) node)
    (multiple-value-bind (context count) (possible-rest-arg-context test)
      (if context
          (instead start test-ctran test-lvar
                   :form `(%rest-true ,test ,context ,count))
          (recurse start test-ctran test-lvar
                   :components (list :test test))))
    (link-node-to-previous-ctran node test-ctran)

    (let ((start-block (ctran-block test-ctran)))
      (setf (block-last start-block) node)
      (ctran-starts-block next)
      (link-blocks start-block then-block) ; TODO accept more block in one call
      (link-blocks start-block else-block))

    (recurse then-ctran next result :components (list :then then))
    (recurse else-ctran next result :components (list :else else))))

;;;; BLOCK and TAGBODY

(define-ir1-translator block ((name forms) instead recurse start next result)
  (start-block start)
  (ctran-starts-block next)
  (let* ((dummy (make-ctran))
         (entry (make-entry))
         (cleanup (make-cleanup :kind :block
                                :mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexenv*)))
    (setf (entry-cleanup entry) cleanup)
    (link-node-to-previous-ctran entry start)
    (use-ctran entry dummy)

    (let* ((env-entry (list entry next result))
           (*lexenv* (make-lexenv :blocks (list (cons name env-entry))
                                  :cleanup cleanup)))
      (ir1-convert-progn-body dummy next result forms))))

(define-ir1-translator return-from ((name value)
                                    instead recurse start next result) ; TODO figure out order of these arguments; ir1-conversion* has start next result instead recurse
  ;; old comment:
  ;;   We make NEXT start a block just so that it will have a block
  ;;   assigned. People assume that when they pass a ctran into
  ;;   IR1-CONVERT as NEXT, it will have a block when it is done.
  ;; KLUDGE: Note that this block is basically fictitious. In the code
  ;;   (BLOCK B (RETURN-FROM B) (SETQ X 3))
  ;; it's the block which answers the question "which block is
  ;; the (SETQ X 3) in?" when the right answer is that (SETQ X 3) is
  ;; dead code and so doesn't really have a block at all. The existence
  ;; of this block, and that way that it doesn't explicitly say
  ;; "I'm actually nowhere at all" makes some logic (e.g.
  ;; BLOCK-HOME-LAMBDA-OR-NULL) more obscure, and it might be better
  ;; to get rid of it, perhaps using a special placeholder value
  ;; to indicate the orphanedness of the code.
  (ctran-starts-block next)
  (let* ((found (or (lexenv-find name blocks)
                    (compiler-error "return for unknown block: ~S" name)))
         (exit-ctran (second found))
         (value-ctran (make-ctran))
         (value-lvar (make-lvar))
         (entry (first found))
         (exit (make-exit :entry entry
                          :value value-lvar)))
    (when (ctran-deleted-p exit-ctran)
      (throw 'locall-already-let-converted exit-ctran))
    (setf (lvar-dest value-lvar) exit)
    (recurse start value-ctran value-lvar :components (list :value value))
    (push exit (entry-exits entry))
    (link-node-to-previous-ctran exit value-ctran)
    (awhen (ctran-home-lambda-or-null start)
      (sset-adjoin entry (lambda-calls-or-closes it)))
    (use-continuation exit exit-ctran (third found))))

(define-ir1-translator tagbody ((tags segments)
                                instead recurse start next result)
  (start-block start)
  (ctran-starts-block next)
  (let* ((dummy (make-ctran))
         (entry (make-entry))
         (cleanup (make-cleanup :kind :tagbody :mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexenv*)))
    (setf (entry-cleanup entry) cleanup)
    (link-node-to-previous-ctran entry start)
    (use-ctran entry dummy)

    (collect ((tag-infos) (starts) (ctrans))
      (starts dummy)
      (loop :for tag     :in tags
            :for segment :in segments :do
         (let* ((tag-ctran (make-ctran))
                (tag-info (list tag entry tag-ctran)))
           (ctrans tag-ctran)
           (starts tag-ctran)
           (ctran-starts-block tag-ctran)
           (tag-infos tag-info)))
      (ctrans next)

      (let ((*lexenv* (make-lexenv :cleanup cleanup :tags (tag-infos))))
        (mapc (lambda (segment start end)
                (ir1-convert-progn-body/new
                 start end (when (eq end next) result) instead recurse
                 (rest segment)))
              segments (starts) (ctrans))))))

(define-ir1-translator go ((tag) instead recurse start next result)
  (ctran-starts-block next)
  (let* ((found (or (lexenv-find tag tags :test #'eql)
                    (compiler-error "attempt to ~S to nonexistent tag: ~S"
                                    'go tag)))
         (entry (first found))
         (exit (make-exit :entry entry)))
    (push exit (entry-exits entry))
    (link-node-to-previous-ctran exit start)
    (awhen (ctran-home-lambda-or-null start)
      (sset-adjoin entry (lambda-calls-or-closes it)))
    (use-ctran exit (second found))))

;;;; Compiler-magic special forms

;;; This handles EVAL-WHEN in non-top-level forms. (EVAL-WHENs in top
;;; level forms are picked off and handled by PROCESS-TOPLEVEL-FORM,
;;; so that they're never seen at this level.)
;;;
;;; ANSI "3.2.3.1 Processing of Top Level Forms" says that processing
;;; of non-top-level EVAL-WHENs is very simple:
;;;
;;;   EVAL-WHEN forms cause compile-time evaluation only at top level.
;;;   Both :COMPILE-TOPLEVEL and :LOAD-TOPLEVEL situation specifications
;;;   are ignored for non-top-level forms. For non-top-level forms, an
;;;   eval-when specifying the :EXECUTE situation is treated as an
;;;   implicit PROGN including the forms in the body of the EVAL-WHEN
;;;   form; otherwise, the forms in the body are ignored.
(define-ir1-translator eval-when ((situations forms)
                                  instead recurse start next result)
  (ir1-convert-progn-body/new
   start next result instead recurse
   (and (nth-value 2 (parse-eval-when-situations situations)) forms))
  (values))

;;; %PRIMITIVE
;;;
;;; Uses of %PRIMITIVE are either expanded into Lisp code or turned
;;; into a funny function.

;;; Carefully evaluate a list of forms, returning a list of the results.
;; (defun eval-info-args (args)
;;   (declare (list args))
;;   (handler-case (mapcar #'eval args)
;;     (error (condition)
;;       (compiler-error "Lisp error during evaluation of info args:~%~A"
;;                       condition))))

;;; Convert to the %%PRIMITIVE funny function. The first argument is
;;; the template, the second is a list of the results of any
;;; codegen-info args, and the remaining arguments are the runtime
;;; arguments.
;;;
;;; We do various error checking now so that we don't bomb out with
;;; a fatal error during IR2 conversion.
;;;
;;; KLUDGE: It's confusing having multiple names floating around for
;;; nearly the same concept: PRIMITIVE, TEMPLATE, VOP. Now that CMU
;;; CL's *PRIMITIVE-TRANSLATORS* stuff is gone, we could call
;;; primitives VOPs, rename TEMPLATE to VOP-TEMPLATE, rename
;;; BACKEND-TEMPLATE-NAMES to BACKEND-VOPS, and rename %PRIMITIVE to
;;; VOP or %VOP.. -- WHN 2001-06-11
;;; FIXME: Look at doing this ^, it doesn't look too hard actually.
(define-ir1-translator %primitive ((name args)
                                   instead recurse start next result)
  (declare (type symbol name))
  (let* ((template (or (gethash name *backend-template-names*)
                       (bug "undefined primitive ~A" name)))
         (required (length (template-arg-types template)))
         (info (template-info-arg-count template))
         (min (+ required info))
         (nargs (length args)))

    (flet ((arg-count-error (name actual-count required-relation required-count)
             (bug "Primitive ~A was called with ~R argument~:P, but ~
                   wants ~A ~R."
                  name actual-count required-relation required-count))
           (template-error (description)
             (bug "~A was used with ~A template."
                  '%primitive description)))
      (if (template-more-args-type template)
          (when (< nargs min)
            (arg-count-error name nargs "at least" min))
          (unless (= nargs min)
            (arg-count-error name nargs "exactly" min)))
      (when (template-conditional-p template)
        (template-error "a conditional"))
      (when (template-more-results-type template)
        (template-error "an unknown values")))

    (instead start next result
             :form `(%%primitive ',template
                                 ',(eval-info-args
                                    (subseq args required min))
                                 ,@(subseq args 0 required)
                                 ,@(subseq args min)))))

(define-ir1-translator quote ((thing) instead recurse start next result)
  (reference-constant/new start next result thing))

(define-ir1-translator %%allocate-closures ((leaves)
                                            instead recurse start next result)
  (aver (null result))
  (let ((lambdas leaves)) ; TODO why this let?
    (instead start next result :form `(%allocate-closures ',lambdas))
    (let ((allocator (node-dest (ctran-next start))))
      (dolist (lambda lambdas)
        (setf (functional-allocator lambda) allocator)))))

;;; FUNCTION and GLOBAL-FUNCTION

;; (defun name-context ()
;;   ;; Name of the outermost non-NIL BLOCK, or the source namestring of
;;   ;; the source file.
;;   (dx-flet ((block-suitable-p (block)
;;               (let ((name (pop block)))
;;                 (and name
;;                      ;; KLUDGE: High debug adds this block on some
;;                      ;; platforms.
;;                      #!-unwind-to-frame-and-call-vop
;;                      (neq 'return-value-tag name)
;;                      ;; KLUDGE: CATCH produces blocks whose cleanup is
;;                      ;; :CATCH.
;;                      (neq :catch
;;                           (cleanup-kind (entry-cleanup (pop block))))))))
;;     (awhen (or (car (find-if #'block-suitable-p (lexenv-blocks *lexenv*)
;;                              :from-end t))
;;                *source-namestring*
;;                (awhen (or *compile-file-truename* *load-truename*)
;;                  (namestring it)))
;;       (list :in it))))

;; (defun name-lambdalike (thing)
;;   (case (car thing)
;;     ((named-lambda)
;;      (or (second thing)
;;          `(lambda ,(third thing) ,(name-context))))
;;     ((lambda)
;;      `(lambda ,(second thing) ,@(name-context)))
;;     ((lambda-with-lexenv)
;;      ;; FIXME: Get the original DEFUN name here.
;;      `(lambda ,(fifth thing)))
;;     (otherwise
;;      (compiler-error "Not a valid lambda expression:~%  ~S" thing))))

(defun call-with-fun-leaf (instead recurse start obtain-leaf-thunk thunk)
  (declare (ignore recurse))
  (multiple-value-bind (leaf allocate-p)
      (funcall obtain-leaf-thunk)
    (if allocate-p
        (let ((new-start (make-ctran)))
          (funcall instead start new-start nil :form `(%%allocate-closures ,leaf))
          (funcall thunk new-start leaf))
        (funcall thunk nil leaf))))

(defmacro with-fun-leaf ((instead recurse start) obtain-leaf
                         (new-start-var leaf-var)
                         &body body)
  `(call-with-fun-leaf ,instead ,recurse ,start
                       (lambda () ,obtain-leaf) ; TODO dx-flet?
                       (lambda (,new-start-var ,leaf-var) ,@body)))

;; TODO there is also the second "mode" (function (lambda (...) ...))
;;
;;      this should definitely also handle the other mode by passing
;;      the components (i.e. lambda-list, declarations, documentation,
;;      body) to fun-name-leaf/new (which is a bad name since it
;;      operates on names and (lambda ...) forms).
(define-ir1-translator function
    ((name                                         ; (function NAME)
      lambda-list declarations documentation body) ; (function (lambda ...))
     instead recurse start next result)
  (with-fun-leaf (instead recurse start)
      (if name
          (find-lexically-apparent-fun name "as the argument to FUNCTION")
          (values (ir1-convert-lambdalike/new
                   instead recurse `(lambda ,lambda-list ,@declarations ,@(when documentation `(,documentation)) ,@body) ; TODO avoid
                   :debug-name (name-lambdalike `(lambda ,lambda-list))) ; TODO debug name
                  t))
      (start leaf)
    (reference-leaf start next result leaf)))

;;; Like FUNCTION, but ignores local definitions and inline
;;; expansions, and doesn't nag about undefined functions.
;;; Used for optimizing things like (FUNCALL 'FOO).
(define-ir1-translator global-function ((thing)
                                        instead recurse start next result)
  (with-fun-leaf (instead recurse start)
      (find-global-fun thing t)
      (start leaf)
    (reference-leaf start next result leaf)))

;;; %FUNCALL

(define-ir1-translator %funcall ((function args)
                                 instead recurse start next result)
  ;; recurse so that (LAMBDA ...) forms arriving here don't get an
  ;; extra cast inserted for them.

  ;; TODO need a helper that only macroexpands
  (let* ((function (recurse start next result :components (list :function function))) ; TODO probably wrong
         (op (when (consp function) (car function))))
    (case op
      (function
       (let ((thing (cdr function))) ; TODO shape of FUNCTION
         (with-fun-leaf (instead recurse start)
             (if (typep thing '(or symbol (cons (eql setf))))
                 (find-lexically-apparent-fun thing "as the argument to %FUNCTION")
                 (break "not implemented")
                 #+no (values (ir1-convert-lambdalike/new
                          instead recurse `(lambda ,lambda-list ,@declarations ,@(when documentation `(,documentation)) ,@body) ; TODO avoid
                          :debug-name (name-lambdalike `(lambda ,lambda-list))) ; TODO debug name
                         t))
             (start leaf)
           (instead start next result :form `(,leaf ,@args)))))
      (global-function
       (let ((thing (cdr function))) ; TODO shape of FUNCTION
         (with-fun-leaf (instead recurse start)
             (find-global-fun thing t)
             (start leaf)
           (instead start next result :form `(,leaf ,@args)))))
      (t
       (let ((ctran (make-ctran))
             (fun-lvar (make-lvar)))
         (instead start ctran fun-lvar :form `(the function ,function))
         (ir1-convert-combination-args/new
          ctran next result instead recurse fun-lvar args))))))

;;;; SYMBOL-MACROLET, LET[*] and LOCALLY

#+TODO (defun symbol-macrolet-definitionize-fun (context)
  (flet ((fail (control &rest args)
           (ecase context
             (:compile (apply #'compiler-error control args))
             (:eval (error 'simple-program-error
                           :format-control control
                           :format-arguments args)))))
    (lambda (name expansion)
      (when (or (boundp name) (eq (info :variable :kind name) :macro))
        (program-assert-symbol-home-package-unlocked
         context name "binding ~A as a local symbol-macro"))
      (let ((kind (info :variable :kind name)))
        (when (member kind '(:special :constant :global))
          (fail "Attempt to bind a ~(~A~) variable with SYMBOL-MACROLET: ~S"
                kind name)))
      ;; A magical cons that MACROEXPAND-1 understands.
      `(,name . (macro . ,expansion)))))

(define-ir1-translator symbol-macrolet ((names expansions declarations body)
                                        instead recurse start next result)
  (signal-if-multiple-definitions 'symbol-macrolet names expansions
                                  :signal-via #'compiler-style-warn)
  (let* ((definitions (mapcar (lambda (name expansion) ; TODO make a function? only this is needed elsewhere
                                (check-variable-name-for-binding
                                 name :compile 'symbol-macrolet)
                                ;; A magical cons that MACROEXPAND-1
                                ;; understands.
                                `(,name . (macro . ,expansion)))
                              names expansions))
         (*lexenv* (make-lexenv :vars definitions)))
                                        ; TODO declarations
    (processing-decls (declarations definitions '() next result)
      (ir1-convert-progn-body/new start next instead recurse result body))))

(define-ir1-translator let ((names values suppliedps declarations body)
                            instead recurse start next result)
  (declare (ignore suppliedps))
  (cond
    ((null names)
     (ir1-translate-locally body start next result)) ; TODO declarations?
    (t
     (signal-if-multiple-definitions 'let names values)
     (binding* ((ctran (make-ctran))
                (fun-lvar (make-lvar))
                (variables (mapcar (lambda (name)
                                     (varify-lambda-arg/new
                                      name "LET as variable name"))
                                   names))
                ((next result)
                 (processing-decls (declarations variables nil next result
                                                 post-binding-lexenv)
                   (let ((fun (ir1-convert-lambda-body/new
                               instead recurse body variables
                               :post-binding-lexenv post-binding-lexenv
                               :debug-name (debug-name 'let (mapcar #'list names values))))) ; TODO necessary?
                     (reference-leaf start ctran fun-lvar fun))
                   (values next result))))
       (ir1-convert-combination-args/new
        ctran next result instead recurse fun-lvar values)))))

(define-ir1-translator let* ((names values suppliedps declarations body)
                             instead recurse start next result)
  (declare (ignore suppliedps))
  (let ((variables (mapcar (lambda (name)
                             (varify-lambda-arg/new
                              name "LET* as variable name"))
                           names)))
    (processing-decls (declarations variables nil next result new-lexenv)
      (ir1-convert-aux-bindings/new
       start next result instead recurse body variables values new-lexenv))))

(define-ir1-translator locally ((declarations body)
                                instead recurse start next result)
  (processing-decls (declarations '() '() next result)
    (ir1-convert-progn-body/new start next result instead recurse body)))


;;;; MACROLET, FLET and LABELS

(defun make-local-functions (context instead recurse
                             names lambda-lists
                             documenations local-declarations bodies)
  (mapcar (lambda (name lambda-list documentation declarations body)
            (ir1-convert-lambda/new
             instead recurse `(lambda ,lambda-list) lambda-list declarations documentation ; TODO form
             `((block ,(fun-name-block-name name)
                 ,body))
             :source-name name
             :maybe-add-debug-catch t
             :debug-name (debug-name context name t)))
          names lambda-lists documenations local-declarations bodies))

(defun ir1-convert-function-bindings (instead recurse start next result
                                      functions body)
  (let ((ctran (make-ctran))
        (dx-p (find-if #'leaf-dynamic-extent functions)))
    (when dx-p
      (ctran-starts-block ctran)
      (ctran-starts-block next))
    (funcall instead start ctran nil :form `(%%allocate-closures ,@functions))
    (cond (dx-p
           (let* ((dummy (make-ctran))
                  (entry (make-entry))
                  (cleanup (make-cleanup :kind :dynamic-extent
                                         :mess-up entry
                                         :info (list (node-dest
                                                      (ctran-next start))))))
             (push entry (lambda-entries (lexenv-lambda *lexenv*)))
             (setf (entry-cleanup entry) cleanup)
             (link-node-to-previous-ctran entry ctran)
             (use-ctran entry dummy)
             (let ((*lexenv* (make-lexenv :cleanup cleanup)))
               (ir1-convert-progn-body/new
                dummy next result instead recurse body))))
          (t
           (ir1-convert-progn-body/new
            ctran next result instead recurse body)))))

#+no (def-ir1-translator macrolet ((definitions &rest body) start next result)
  #!+sb-doc
  "MACROLET ({(name lambda-list form*)}*) body-form*

Evaluate the BODY-FORMS in an environment with the specified local macros
defined. NAME is the local macro name, LAMBDA-LIST is a DEFMACRO style
destructuring lambda list, and the FORMS evaluate to the expansion."
  (funcall-in-macrolet-lexenv
   definitions
   (lambda (&key funs)
     (declare (ignore funs))
     (ir1-translate-locally body start next result))
   :compile))

(define-ir1-translator flet ((names lambda-lists
                              documentations local-declarations bodies
                              declarations body)
                             instead recurse start next result)
  (let ((functions (make-local-functions
                    'flet instead recurse
                    names lambda-lists
                    documentations local-declarations bodies)))
    (processing-decls (declarations '() functions next result)
      (let ((*lexenv* (make-lexenv :funs (pairlis names functions))))
        (ir1-convert-function-bindings
         instead recurse start next result functions body)))))

#+TODO (define-ir1-translator labels ((names lambda-lists
                                documentations local-declarations bodies
                                declarations body)
                               instead recurse start next result)
  (let* (;; dummy LABELS functions, to be used as placeholders
         ;; during construction of real LABELS functions
         (placeholder-funs (mapcar (lambda (name)
                                     (make-functional
                                      :%source-name name
                                      :%debug-name (debug-name
                                                    'labels-placeholder
                                                    name)))
                                   names))
         ;; (like PAIRLIS but guaranteed to preserve ordering:)
         (placeholder-fenv (mapcar #'cons names placeholder-funs))
         ;; the real LABELS functions, compiled in a LEXENV which
         ;; includes the dummy LABELS functions
         (real-funs
          (let ((*lexenv* (make-lexenv :funs placeholder-fenv)))
            (make-local-functions
             'flet names lambda-lists
             documentations local-declarations bodies))))

    ;; Modify all the references to the dummy function leaves so
    ;; that they point to the real function leaves.
    (loop for real-fun in real-funs and
       placeholder-cons in placeholder-fenv do
         (substitute-leaf real-fun (cdr placeholder-cons))
         (setf (cdr placeholder-cons) real-fun))

    ;; Voila.
    (processing-decls (declarations nil real-funs next result)
      (let ((*lexenv* (make-lexenv
                       ;; Use a proper FENV here (not the
                       ;; placeholder used earlier) so that if the
                       ;; lexical environment is used for inline
                       ;; expansion we'll get the right functions.
                       :funs (pairlis names real-funs))))
        (ir1-convert-fbindings start next result real-funs forms)))))


;;;; [TRULY-]THE

;;; A logic shared among THE and TRULY-THE.
(defun the-in-policy/new (instead recurse start next result
                          type value policy)
  (declare (ignore instead))
  (let ((type (if (ctype-p type)
                  type
                  (compiler-values-specifier-type type))))
    (if (or (eq type *wild-type*)
            (eq type *universal-type*)
            (and (leaf-p value)
                 (values-subtypep
                  (make-single-value-type (leaf-type value))
                  type))
            (and (sb!xc:constantp value)
                 (or (not (values-type-p type))
                     (values-type-may-be-single-value-p type))
                 (ctypep (constant-form-value value)
                         (single-value-type type))))
        (funcall recurse start next result :components (list :form value))
        (let ((value-ctran (make-ctran))
              (value-lvar (make-lvar)))
          (funcall recurse start value-ctran value-lvar
                   :components (list :form value))
          (let ((cast (make-cast value-lvar type policy)))
            (link-node-to-previous-ctran cast value-ctran)
            (setf (lvar-dest value-lvar) cast)
            (use-continuation cast next result))))))

(define-ir1-translator the ((value-type form)
                            instead recurse start next result)
  (the-in-policy/new instead recurse start next result
                     value-type form (lexenv-policy *lexenv*)))

;;; This is like the THE special form, except that it believes
;;; whatever you tell it. It will never generate a type check, but
;;; will cause a warning if the compiler can prove the assertion is
;;; wrong.
;;;
(define-ir1-translator truly-the ((value-type form)
                                  instead recurse start next result)
  (the-in-policy/new instead recurse start next result
                     value-type form **zero-typecheck-policy**))

;;;; SETQ

(defun explode-setq/new (operator names-and-value-forms)
  `(progn
     ,@(loop :for (name value-form) :on names-and-value-forms :by #'cddr
          :collect `(,operator ,name ,value-form))))

(define-ir1-translator setq ((names value-forms)
                             instead recurse start next result)
  (if (= (length names) 1)
      (let* ((name (first names))
             (value-form (first value-forms))
             (leaf (or (lexenv-find name vars) (find-free-var name))))
        (etypecase leaf
          (leaf
           (when (constant-p leaf)
             (compiler-error "~S is a constant and thus can't be set." name))
           (when (lambda-var-p leaf)
             (awhen (ctran-home-lambda-or-null start)
               (sset-adjoin leaf (lambda-calls-or-closes it)))
             (when (lambda-var-ignorep leaf)
               ;; ANSI's definition of "Declaration IGNORE, IGNORABLE"
               ;; requires that this be a STYLE-WARNING, not a full warning.
               (compiler-style-warn
                "~S is being set even though it was declared to be ignored."
                name)))
           (if (and (global-var-p leaf) (eq :unknown (global-var-kind leaf)))
               ;; For undefined variables go through SET, so that we can catch
               ;; constant modifications.
               (instead start next result :form `(set ',name ,value-form))
               (setq-var/new instead recurse start next result leaf value-form)))
          (cons
           (aver (eq (car leaf) 'macro))
           ;; Allow *MACROEXPAND-HOOK* to see NAME get expanded, not
           ;; just see a use of SETF on the new place.
           (instead start next result
                    :form `(setf ,name ,value-form)))
          (heap-alien-info
           (instead start next result
                    :form `(%set-heap-alien ',leaf ,value-form)))))
      ;; Convert to a sequence of single-assignment SETQs.
      (instead start next result
               :form (explode-setq/new 'setq (mapcan #'list names value-forms)))))

;;; This is kind of like REFERENCE-LEAF, but we generate a SET node.
;;; This should only need to be called in SETQ.
(defun setq-var/new (instead recurse start next result var value)
  (declare (type ctran start next)
           (type (or lvar null) result)
           (type basic-var var)
           (ignore recurse))
  (let ((dest-ctran (make-ctran))
        (dest-lvar (make-lvar))
        (type (or (lexenv-find var type-restrictions)
                  (leaf-type var))))
    (funcall instead start dest-ctran dest-lvar
             :form `(the ,(type-specifier type) ,value))
    (let ((res (make-set :var var :value dest-lvar)))
      (setf (lvar-dest dest-lvar) res
            (leaf-ever-used var) t)
      (push res (basic-var-sets var))
      (link-node-to-previous-ctran res dest-ctran)
      (use-continuation res next result))))

;;;; CATCH, THROW and UNWIND-PROTECT

;;; We turn THROW into a MULTIPLE-VALUE-CALL of a magical function
;;; %THROW, since as far as IR1 is concerned, it has no interesting
;;; properties other than receiving multiple-values.
(define-ir1-translator throw ((tag result-form)
                              instead recurse start next result)
  (instead start next result
           :form `(multiple-value-call #'%throw ,tag ,result-form)))

(define-ir1-translator %within-cleanup ((kind mess-up body)
                                        instead recurse start next result)
  (let ((dummy (make-ctran))
        (dummy2 (make-ctran)))
    (recurse start dummy nil :components (list :mess-up mess-up))
    (let* ((mess-node (ctran-use dummy))
           (cleanup (make-cleanup :kind kind
                                  :mess-up mess-node))
           (old-cup (lexenv-cleanup *lexenv*))
           (*lexenv* (make-lexenv :cleanup cleanup)))
      (setf (entry-cleanup (cleanup-mess-up old-cup)) cleanup)
      (instead dummy dummy2 nil :form '(%cleanup-point))
      (ir1-convert-progn-body/new dummy2 next result instead recurse body))))

(define-ir1-translator %escape-fun ((tag)
                                    instead recurse start next result)
  (let ((fun (ir1-convert-lambda
              `(lambda ()
                 (return-from ,tag (%unknown-values)))
              :debug-name (debug-name 'escape-fun tag)))
        (ctran (make-ctran)))
    (setf (functional-kind fun) :escape)
    (instead start ctran nil :form `(%%allocate-closures ,fun))
    (reference-leaf ctran next result fun)))

(define-ir1-translator %cleanup-fun ((name)
                                     instead recurse start next result)
  ;; FIXME: Should this not be :TEST #'EQUAL? What happens to
  ;; (SETF FOO) here?
  (let ((fun (lexenv-find name funs)))
    (aver (lambda-p fun))
    (setf (functional-kind fun) :cleanup)
    (reference-leaf start next result fun)))

(define-ir1-translator catch ((tag body)
                              instead recurse start next result)
  ;; We represent the possibility of the control transfer by making an
  ;; "escape function" that does a lexical exit, and instantiate the
  ;; cleanup using %WITHIN-CLEANUP.
  (instead start next result
           :form (with-unique-names (exit-block)
                   `(block ,exit-block
                      (%within-cleanup
                       :catch (%catch (%escape-fun ,exit-block) ,tag)
                       ,@body)))))

(define-ir1-translator unwind-protect ((protected cleanup)
                                       instead recurse start next result)
  ;; UNWIND-PROTECT is similar to CATCH, but hairier. We make the
  ;; cleanup forms into a local function so that they can be
  ;; referenced both in the case where we are unwound and in any local
  ;; exits. We use %CLEANUP-FUN on this to indicate that reference by
  ;; %UNWIND-PROTECT isn't "real", and thus doesn't cause creation of
  ;; an XEP.
  (instead
   start next result
   :form (with-unique-names (cleanup-fun drop-thru-tag exit-tag next start count) ; TODO separate function for generating this form
           `(flet ((,cleanup-fun ()
                     ,@cleanup
                     nil))
              ;; FIXME: If we ever get DYNAMIC-EXTENT working, then
              ;; ,CLEANUP-FUN should probably be declared DYNAMIC-EXTENT,
              ;; and something can be done to make %ESCAPE-FUN have
              ;; dynamic extent too.
              (declare (dynamic-extent #',cleanup-fun)) ; TODO shouldn't this resolve the above FIXME?
              (block ,drop-thru-tag
                (multiple-value-bind (,next ,start ,count)
                    (block ,exit-tag
                      (%within-cleanup
                       :unwind-protect
                       (%unwind-protect (%escape-fun ,exit-tag)
                                        (%cleanup-fun ,cleanup-fun))
                       (return-from ,drop-thru-tag ,protected)))
                  (declare (optimize (insert-debug-catch 0)))
                  (,cleanup-fun)
                  (%continue-unwind ,next ,start ,count)))))))

;;;; multiple-value stuff

(define-ir1-translator multiple-value-call ((function-form args)
                                            instead recurse start next result)
  (let* ((ctran (make-ctran))
         (fun-lvar (make-lvar))
         (node (if args
                   ;; If there are arguments, MULTIPLE-VALUE-CALL
                   ;; turns into an MV-COMBINATION.
                   (make-mv-combination fun-lvar)
                   ;; If there are no arguments, then we convert to a
                   ;; normal combination, ensuring that a MV-COMBINATION
                   ;; always has at least one argument. This can be
                   ;; regarded as an optimization, but it is more
                   ;; important for simplifying compilation of
                   ;; MV-COMBINATIONS.
                   (make-combination fun-lvar))))
    (instead start ctran fun-lvar
             :form (ensure-source-fun-form function-form))
    (setf (lvar-dest fun-lvar) node)
    (collect ((arg-lvars))
      (let ((this-start ctran))
        (dolist (arg args)
          (let ((this-ctran (make-ctran))
                (this-lvar (make-lvar node)))
            (funcall instead this-start this-ctran this-lvar :form arg)
            (setf this-start this-ctran)
            (arg-lvars this-lvar)))
        (link-node-to-previous-ctran node this-start)
        (use-continuation node next result)
        (setf (basic-combination-args node) (arg-lvars))))))

(define-ir1-translator multiple-value-prog1 ((values-form forms)
                                             instead recurse start next result)
  (let ((dummy (make-ctran)))
    (ctran-starts-block dummy)
    (recurse start dummy result :components (list :values-form values-form)) ; TODO simplify
    (ir1-convert-progn-body/new dummy next nil instead recurse forms)))

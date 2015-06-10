#.(progn
    (ql:quickload :log4cl)
    (values))

(cl:in-package #:cl-user)

;;;; Shortcuts

;;; Convenience wrapper for walking with
;;; * lexenv updates and lookups (see WALK-WITH-LEXENV-LOOKUP)
;;; * processing of hooks (via ADD-HOOK-PROCESSING; see end of example
;;;   for actual hooks)
;;; * reconstituting possibly transformed forms (done by the anonymous
;;;   "primary" walk function in the body of this function)
(defun walk-with-hooks (hooks form)
  (walk-with-lexenv-lookup
   ;; Augment "primary" walk function that basically does nothing but
   ;; log macro expansions, reconstitute everything and, most
   ;; importantly, execute hooks.
   (sb-c:add-hook-processing
    (lambda (instead recurse form kind name &rest components)
      (declare (ignore instead))
      (log:info form kind name components)
      (typecase kind
        (sb-c:leaf-info
         name)
        (sb-c:macroid-info
         (let ((expansion (funcall recurse)))
           (log:info form "=>" expansion)
           expansion))
        (t
         (apply #'sb-c:reconstitute form kind name
                (append (funcall recurse) components))))))
   (reduce (lambda (hook form)
             (sb-c:wrap hook form))
           hooks :initial-value form :from-end t)))

;;; Convenience wrapper for walking with lexenv updates and lookups.
(defun walk-with-lexenv-lookup (function form)
  (let* ((env (sb-c::coerce-to-lexenv nil))
         (sb-c::*lexenv* env))
    (sb-c:walk-forms
     'list
     (lambda (instead recurse form kind name &rest components)
       (let* ((old-env sb-c::*lexenv*)
              (env (sb-c::make-lexenv))
              (sb-c::*lexenv* env))
         (apply #'maybe-note-lexical-thing
                env old-env instead recurse form kind name components)
         (apply function instead recurse form kind name components)))
     form
     (lambda (form)
       (sb-c:classify-variable-form form sb-c::*lexenv*))
     (lambda (form)
       (sb-c:classify-application-form form sb-c::*lexenv*))
     (lambda (kind expander form)
       (sb-c:expand-macro kind expander form sb-c::*lexenv*)))))

;;; Convenience wrapper for walking with lexenv updates and lookups.
(defun maybe-note-lexical-thing (env old-env instead recurse
                                 form kind name &rest components)
  (declare (ignore recurse form))
  (when (typep kind 'sb-c:special-operator-info)
    (case name
      ((flet labels)
       (destructuring-bind
             (&key names lambda-lists documentations local-declarations bodies
                   &allow-other-keys)
           components
         (mapc (lambda (&rest args)
                 (apply #'sb-c:note-lexical-function
                        (append args (list env))))
               names lambda-lists documentations local-declarations bodies)))
      (macrolet
       (destructuring-bind
             (&key names lambda-lists local-declarations bodies
                   &allow-other-keys)
           components
         (mapc (lambda (&rest args)
                 (apply #'sb-c:note-lexical-macro
                        (append args (list env old-env instead))))
               names lambda-lists local-declarations bodies)))
      (symbol-macrolet
       (destructuring-bind (&key names expansions) components
        (mapc (lambda (name expansion)
                (sb-c:note-lexical-symbol-macro name expansion env))
              names expansions))))))

;;;; Example hooks

;;; Example hook which traces execution by BREAKing at variable
;;; accesses, function calls and special operator evaluation (traces
;;; "through" macros).
(defun tracer (wrap recurse form kind name &rest components)
  (log:info form kind name components)
  (typecase kind
    ;; Do not modify constants and (compiler-internal) leaf
    ;; structures.
    ((or sb-c:%leaf-info sb-c:self-evaluating-info)
     form)
    ;; Print values of variables.
    (sb-c:variable-info
     (sb-int:with-unique-names (value)
       `(let ((,value ,form))
          (break "Value of ~S is ~S" ',name ,value)
          ,value)))
    ;; Expand all kinds of macros and process the expansion.
    (sb-c:macroid-info
     (apply wrap form kind name components))
    ;; Print applications of special operators, named functions and
    ;; LAMBDA forms.
    (t
     `(progn
        (break "About to evaluate ~S" ',form)
        ,(apply #'sb-c:reconstitute form kind name
                (append (apply #'wrap-components
                               wrap recurse form kind name components)
                        components))))))

;;; Another example hook which modifies certain function calls.
;;;
;; See comments in TRACER function.
(defun trickster (wrap recurse form kind name &rest components)
  (let ((sb-c::*walk-mode* :old))
    (log:info form kind name components))
  (typecase kind
    (sb-c:leaf-info
     form)
    (sb-c:macroid-info
     (apply wrap form kind name components))
    (t
     (let ((replacement (when (typep kind 'sb-c:named-application-info)
                          (cdr (assoc name '((1+ . 1-) (1- . 1+)
                                             (+  . -)  (-  . +)
                                             (*  . /)  (/  . *)))))))
       (apply #'sb-c:reconstitute ; TODO this is probably a common pattern
              form kind (or replacement name)
              (append (apply #'wrap-components
                             wrap recurse form kind name components)
                      components))))))

;;; Helper for above hooks.
(defun wrap-components (wrap recurse form kind name &rest components)
  (declare (ignore form))
  (let (;; In case of the MACROLET special operator, make sure to not
        ;; process the bodies of the expander functions but only the
        ;; body of the MACROLET itself.
        (maybe-components-argument
         (when (and (typep kind 'sb-c:special-operator-info)
                    (eq name 'macrolet))
           (list :components (list :body (getf components :body))))))
    (apply recurse
           :function (lambda (instead recurse form kind name &rest components)
                       (declare (ignore instead recurse))
                       (apply wrap form kind name components))
           maybe-components-argument)))

;;;; Examples

;;; Note how the hooks compose sanely in the following two examples.
(eval (walk-with-hooks
       '(tracer trickster)
       `(let ((a 1))
          (zerop (1+ a)))))

(eval (walk-with-hooks
       '(trickster tracer)
       `(let ((a 1))
          (zerop (1+ a)))))

;; walking into RETURN requires macro expansion
(eval (walk-with-hooks
       '(tracer trickster)
       `(let ((a 1))
          (block nil (return (1+ a))))))

(eval (walk-with-hooks
       '(tracer)
       `(let ((a (lambda () (1+ 1))))
          (funcall a))))

(walk-with-hooks '() `(locally
                          (declare (type boolean a))
                        (progn
                          (if a
                              ,(sb-c:wrap #'tracer '(1+ 1))
                              (return b)))))

(eval (walk-with-hooks
       '(tracer)
       `(let ((a t))
          (locally
              (declare (type boolean a))
            (progn (if a
                       ((lambda () (1+ 1)))
                       (return b)))))))

(eval (walk-with-hooks
       '(tracer)
       `(flet ((foo ()
                 (declare (type bar x))
                 (1+ 1)))
          (foo))))

;; SETQ is the only way to encounter a variable with :ACCESS :WRITE.
(eval (walk-with-hooks
       '(tracer trickster)
       `(let ((foo))
          (setq foo 1))))

;; walking into the symbol-macro requires symbol-macro expansion
(define-symbol-macro foofoofoo (cdr foo))
(eval (walk-with-hooks
       '(tracer trickster)
       `(let ((foo (cons :a '(1 2 3))))
          (block nil (return (1+ (length foofoofoo)))))))

(eval (walk-with-hooks
       '(tracer trickster)
       `(let ((foo (cons :a 3))) (setq foofoofoo (1+ 1)) foo)))

(eval (walk-with-hooks
       '(tracer trickster)
       `(let ((foo (cons :a 0))) (incf foofoofoo) foo)))

(eval (walk-with-hooks
       '(tracer trickster)
       `(let ((foo (cons :a 0)))
          (macrolet ((foofoofoo () `(cdr foo)))
            (incf (foofoofoo))
            foo))))

(eval (walk-with-hooks '(tracer) 'sb-ext:*evaluator-mode*))

(eval (walk-with-hooks '(tracer) `(flet ((foo (x) (1+ x)))
                                    (foo 5))))

(eval (walk-with-hooks
       '(tracer trickster)
       `(block nil
          (macrolet ((foo (x) `(return (1+ ,x))))
            (foo 5)))))


(eval (walk-with-hooks '(tracer trickster)
                       '#.(find-class 'standard-class)))

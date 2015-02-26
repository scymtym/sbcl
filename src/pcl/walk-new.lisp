;;;; a simple code walker
;;;;
;;;; TODO talk about compatibility

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB!WALKER")

(define-deprecated-variable :early "1.2.13" *walk-form-expand-macros-p*
  :value nil)

(define-deprecated-function :early "1.2.13" define-walker-template ? ()
  )

(define-deprecated-function :early "1.2.13" var-declaration ? (declaration var env)
  )

(define-deprecated-function :early "1.2.13" var-globally-special-p ? ()
  )

(define-deprecated-function :early "1.2.13" var-lexical-p ? (var env)
  )

(define-deprecated-function :early "1.2.13" var-special-p ? (var env)
  )

(define-deprecated-function :early "1.2.13" walk-form ? ()
  )


;;;; the actual walker

(defvar *walk-form-expand-macros-p* nil)

(defun walk-form/new (form
                  &optional
                  environment
                  (walk-function
                   (lambda (subform context env)
                     #+no (declare (ignore context env))
                     (print (list context subform (var-lexical-p/new 'a env)))
                     subform)))
  (let ((sb-c:*lexenv* (sb-c::coerce-to-lexenv environment))) #+no walker-environment-bind #+no  (new-env environment :walk-function walk-function)
    (dx-flet ((classify-variable (form)
                (sb-c:classify-variable-form form sb-c:*lexenv*))
              (classify-application (form)
                (sb-c:classify-application-form form sb-c:*lexenv*))
              (expand-macro (kind expander form)
                (sb-c:expand-macro kind expander form sb-c:*lexenv*))
              (one-form (instead recurse reconstitute form kind name &rest components)
                (let ((sb-c:*lexenv* (sb-c::make-lexenv))
                      (context       :eval))
                 (typecase kind
                   (sb-c:variable-info
                    (when (eq (getf components :access) :bind)
                      (sb-c:note-lexical-variable name sb-c:*lexenv*)
                      (setf context :set))))
                 (multiple-value-bind (new-form walk-no-more-p)
                     (funcall walk-function form context sb-c:*lexenv*)
                   (cond
                     (walk-no-more-p
                      new-form)
                     ((not (eq form new-form))
                      (funcall instead :form new-form))
                     (t
                      (funcall recurse)))))))
      (sb-c:walk-forms/reconstitute
       #'one-form form
       #'classify-variable #'classify-application #'expand-macro))))

(walk-form/new '(let ((a 1)) a))

(defun var-declaration (name environment)
  )

(defun var-lexical-p/new (name environment)
  (multiple-value-bind (kind where type plist)
      (sb-c:classify-variable-form name environment)
    (declare (ignore type plist))
    (and (eq kind :variable) (eq where :lexical))))

(defun var-special-p/new (name environment) ; global or lexical
  (multiple-value-bind (kind where type plist)
      (sb-c:classify-variable-form name environment)
    (declare (ignore where type))
    (and (eq kind :variable) (getf plist :special))))

(defun var-globally-symbol-macro-p/new (name)
  (multiple-value-bind (kind where type plist)
      (sb-c:classify-variable-form name sb-c:*lexenv*) ; TODO use /global variant to ignore lexical context?
    (declare (ignore type plist))
    (and (eq kind :symbol-macro) (eq where :global))))

(defun var-globally-special-p/new (name)
  (multiple-value-bind (kind where type plist)
      (sb-c:classify-variable-form name sb-c:*lexenv*) ; TODO use /global variant to ignore lexical context?
    (declare (ignore type))
    (and (eq kind :variable) (eq where :global) (getf plist :special))))

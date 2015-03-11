#.(progn
    (sb-ext:unlock-package :cl)
    (values))

(cl:in-package "SB-C")


;;;; Hack to switch between old and new ir1 conversion.
;;;;
;;;; Example:
;;;;
;;;;   (let ((sb-c::*walk-mode* :new))
;;;;     (compile nil '(lambda (foo) (funcall foo))))

(defvar *walk-mode* :old)

;;;; SPECIAL-OPERATOR-P

(setf (fdefinition 'special-operator-p/old)
      (fdefinition 'special-operator-p))

(defun special-operator-p (symbol)
  (ecase *walk-mode*
    (:old (special-operator-p/old symbol))
    (:new (special-operator-p/new symbol))))

(defun special-operator-p/new (symbol)
  (declare (symbol symbol))
  (find-special-operator-info symbol))

;;;; IR1-CONVERT

(setf (fdefinition 'ir1-convert/old)
      (fdefinition 'ir1-convert))

(defun ir1-convert (start next result form)
  (funcall (ecase *walk-mode*
             (:old #'ir1-convert/old)
             (:new 'ir1-convert/new))
           start next result form))

;;;; MAKE-FUNCTIONAL-FROM-TOPLEVEL-LAMBDA

(setf (fdefinition 'make-functional-from-toplevel-lambda/old)
      (fdefinition 'make-functional-from-toplevel-lambda))

(defun make-functional-from-toplevel-lambda (lambda-expression
                                             &rest args &key
                                                          name
                                                          path)
  (declare (ignore name path))
  (apply (ecase *walk-mode*
           (:old #'make-functional-from-toplevel-lambda/old)
           (:new 'make-functional-from-toplevel-lambda/new))
         lambda-expression args))

;; TODO should not be in this file
(defun make-functional-from-toplevel-lambda/new (lambda-expression
                                                 &key
                                                 name
                                                 (path
                                                  ;; I'd thought NIL should
                                                  ;; work, but it doesn't.
                                                  ;; -- WHN 2001-09-20
                                                  (missing-arg)))
  (let* ((*current-path* path)
         (component (make-empty-component))
         (*current-component* component)
         (debug-name-tail (or name (name-lambdalike lambda-expression)))
         (source-name (or name '.anonymous.)))
    (setf (component-name component) (debug-name 'initial-component debug-name-tail)
          (component-kind component) :initial)
    (let* ((fun (walk-forms-for-ir1-conversion
                 (lambda (instead recurse form info &rest args)
                   (declare (ignore info args))
                   ; TODO (aver (typep info '))
                   (with-node-arguments (make-functional-from-toplevel-lambda instead recurse)
                     (ir1-convert-lambdalike/new
                      (back-to-ir1-convert instead)
                      (back-to-ir1-convert recurse)
                      form :source-name source-name)))
                 lambda-expression))
           ;; Convert the XEP using the policy of the real function. Otherwise
           ;; the wrong policy will be used for deciding whether to type-check
           ;; the parameters of the real function (via CONVERT-CALL /
           ;; PROPAGATE-TO-ARGS). -- JES, 2007-02-27
           (*lexenv* (make-lexenv :policy (lexenv-policy (functional-lexenv fun))))
           (xep (walk-forms-for-ir1-conversion
                 (lambda (instead recurse form info name
                          &rest components
                          &key lambda-list declarations documentation body
                          &allow-other-keys)
                   (declare (ignore components))
                   (etypecase info
                     (macro-info
                      (funcall recurse))
                     (special-operator-info ; (function (lambda () …)
                      (aver (eq name 'function))
                      (with-node-arguments (make-functional-from-toplevel-lambda instead recurse) ; TODO macro expansion is duplicated above
                        (ir1-convert-lambda/new
                         (back-to-ir1-convert instead)
                         (back-to-ir1-convert recurse)
                         form lambda-list declarations documentation body
                         :source-name source-name
                         :debug-name (debug-name 'tl-xep debug-name-tail)
                         :system-lambda t)))))
                 (make-xep-lambda-expression fun))))
      (when name
        (assert-global-function-definition-type name fun))
      (setf (functional-kind xep) :external
            (functional-entry-fun xep) fun
            (functional-entry-fun fun) xep
            (component-reanalyze component) t
            (functional-has-external-references-p xep) t)
      (reoptimize-component component :maybe)
      (locall-analyze-xep-entry-point fun)
      ;; Any leftover REFs to FUN outside local calls get replaced with the
      ;; XEP.
      (substitute-leaf-if (lambda (ref)
                            (let* ((lvar (ref-lvar ref))
                                   (dest (when lvar (lvar-dest lvar)))
                                   (kind (when (basic-combination-p dest)
                                           (basic-combination-kind dest))))
                              (neq :local kind)))
                          xep
                          fun)
      xep)))

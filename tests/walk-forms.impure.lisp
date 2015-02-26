;;;; Test the WALK-FORMS* family of functions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(cl:in-package #:cl-user)

(load "assertoid.lisp")
(use-package "ASSERTOID")

;;;; Utilities

(defun maybe-note-lexical-thing (env old-env instead recurse
                                 form kind name &rest components)
  (declare (ignore recurse form))
  (typecase kind
    (sb-c:variable-info ; TODO good idea? handle the let/let* instead?
     #+no (print (list name :in env components))
     (when (eq (getf components :access) :bind)
       #+no (print (list :binding name :in env))
       (sb-c:note-lexical-variable name env)))
    (sb-c:special-operator-info
     (case name
       ((flet labels)
        (mapc (lambda (&rest args)
                (apply #'sb-c:note-lexical-function
                       (append args (list env))))
              (getf components :names)
              (getf components :lambda-lists)
              (getf components :documentations)
              (getf components :local-declarations)
              (getf components :bodies)))
       (macrolet
           (mapc (lambda (&rest args) ; TODO destructuring-bind (&key names ...)
                   (apply #'sb-c:note-lexical-macro
                          (append args (list env old-env instead))))
                 (getf components :names)
                 (getf components :lambda-lists)
                 (getf components :local-declarations)
                 (getf components :bodies)))
       (symbol-macrolet
           (mapc (lambda (name expansion)
                   (sb-c:note-lexical-symbol-macro name expansion env))
                 (getf components :names)
                 (getf components :expansions)))))))

(defun walk-with-lexenv-lookup (function form &key environment)
  (let* ((env (sb-c::coerce-to-lexenv environment))
         (sb-c::*lexenv* env))
    (sb-c:walk-forms
     'list
     (lambda (instead recurse form kind name &rest components)
       (let* ((old-env sb-c::*lexenv*)
              (env (typecase kind ; TODO make accessor operator-info-establishes-lexical-environment-p?
                     (sb-c:leaf-info old-env)
                     (t (sb-c::make-lexenv))))
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

(defun identity-walk-with-lexenv-lookup (form &key environment)
  (walk-with-lexenv-lookup
   (lambda (instead recurse form kind name &rest components)
     (declare (ignore instead))
     #+no (print (list form kind name components))
     (typecase kind
       ((or sb-c:leaf-info sb-c:symbol-macro-info) ; TODO can we avoid this?
        name)
       (sb-c:macroid-info
        :macro)
       (t
        (apply #'sb-c:reconstitute form kind name
               (append (progn #+no print (funcall recurse)) (progn #+no print components))))))
   form :environment environment))

(defun recording-walk-with-lexenv-lookup (form &key environment)
  (walk-with-lexenv-lookup
   (lambda (instead recurse form kind name &rest components)
     (declare (ignore instead))
     (list* (list form kind name components)
            (funcall recurse)))
   form :environment environment))

(defun plist-alist (plist)
  (loop :for (key value) :on plist :by #'cddr
     :collect (cons key value)))

(defun %check-walk-cases (environment &rest cases)
  (labels ((check-form (info expected)
             (destructuring-bind ((form kind name &optional plist)
                                  &rest subforms)
                 info
               (destructuring-bind ((expected-kind expected-name
                                     &optional expected-plist)
                                    &rest expected-subforms)
                   expected
                 ;; Compare KIND, NAME and PLIST.
                 (assert (typep kind expected-kind))
                 (assert (eq name expected-name))
                 (let* ((alist (plist-alist plist))
                        (expected-alist (plist-alist expected-plist))
                        (diff1 (set-difference alist expected-alist
                                               :key #'car :test #'equal))
                        (diff2 (set-difference expected-alist alist
                                               :key #'car :test #'equal)))
                   (when diff1
                     (error "~@<In ~S: unexpected properties ~{~S~^, ~}~@:>"
                            form diff1))
                   (when diff2
                     (error "~@<In ~S: missing properties ~{~S~^, ~}~@:>"
                            form diff2))
                   (loop :for (key . value) :in alist :do
                      (let ((expected-value (cdr (assoc key expected-alist))))
                        (case expected-value
                          (:ignore)
                          (t
                           (unless (equal value expected-value)
                             (error "~@<In ~S: unexpected value of ~S ~
                                     property; actual: ~S, expected: ~
                                     ~S~@:>"

                                    form key value expected-value)))))))
                 ;; Recurse into subforms.
                 ;; TODO same diff algorithm as for properties?
                 (cond
                   ((subtypep expected-kind 'sb-c:macroid-info)
                    (check-form subforms (first expected-subforms))
                    (setf expected-subforms nil))
                   (t
                    (loop :for (key subform) :on subforms :by #'cddr :do
                       (let ((expected-subform
                              (getf expected-subforms key :missing)))
                         (case expected-subform
                           (:missing
                            (error "~@<In ~S: unexpected subform ~S ~S.~@:>"
                                   form key subform))
                           (t
                            (if (keywordp (second (first subform))) ; hack
                                (check-form subform expected-subform)
                                (mapc #'check-form subform expected-subform)))))
                       (remf expected-subforms key))))
                 (when expected-subforms
                   (error "~@<In ~S: missing subforms ~{~S ~S~^, ~}~@:>"
                          form expected-subforms))))))
    (mapc (lambda (case)
            (destructuring-bind (form expected) case
              ;; Check roundtrip.
              (let ((result (identity-walk-with-lexenv-lookup
                             form :environment environment)))
                (case result
                  (:macro)
                  (t (assert (equal result form)))))
              ;; Check walked forms.
              (let ((info (recording-walk-with-lexenv-lookup
                           form :environment environment)))
                (assert (typep info '(cons )))
                (check-form info expected))))
          cases)))

(defun check-walk-cases (&rest cases)
  (handler-bind ((error (lambda (c) (sb-debug:print-backtrace))))
   (apply #'%check-walk-cases nil cases)))

(defun check-walk-cases/environment (environment &rest cases)
  (apply #'%check-walk-cases environment cases))

;;;; Kind VARIABLE-INFO

(defvar walk-forms.variable.global.no-type 1)

(declaim (type integer walk-forms.variable.global.type))
(defvar walk-forms.variable.global.type 1)

(with-test (:name (sb-c:walk-forms sb-c:variable-info :global))
  (check-walk-cases
   '(walk-forms.variable.global.no-type
     ((sb-c:variable-info walk-forms.variable.global.no-type
       (:where :global  :type t :access :read))))
   '(walk-forms.variable.global.type
     ((sb-c:variable-info walk-forms.variable.global.type
       (:where :global :type integer :access :read))))))

(defconstant walk-forms.constant.global.no-type :foo)

(declaim (type keyword walk-forms.constant.global.type))
(defconstant walk-forms.constant.global.type :foo)

(with-test (:name (sb-c:walk-forms sb-c:variable-info :global :constant))
  (check-walk-cases
   '(walk-forms.constant.global.no-type
     ((sb-c:variable-info walk-forms.constant.global.no-type
       (:where :global :type t :constant t :access :read))))
   '(walk-forms.constant.global.type
     ((sb-c:variable-info walk-forms.constant.global.type
       (:where :global :type keyword :constant t :access :read))))))

(with-test (:name (sb-c:walk-forms sb-c:variable-info :global :unknown))
  (check-walk-cases
   '(walk-forms.variable.global.unknonwn
     ((sb-c:variable-info walk-forms.variable.global.unknonwn
       (:where nil :access :read))))))

(with-test (:name (sb-c:walk-forms sb-c:variable-info :lexical))
  ;; TODO
  )

;;;; Kind SELF-EVALUATING-INFO

(with-test (:name (sb-c:walk-forms sb-c:self-evaluating-info))
  ;; TODO
  (check-walk-cases
   '(1 ((sb-c:self-evaluating-info 1 ())))))


;;;; Kind SPECIAL-OPERATOR-INFO


;;;; Special operators for control

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info progn))
  ;; TODO
  (check-walk-cases
   '((progn)
     ((sb-c:special-operator-info progn ())))
   '((progn 1)
     ((sb-c:special-operator-info progn (:forms (1)))
      :forms
      (((sb-c:self-evaluating-info 1 ())))))
   '((progn 1 2)
     ((sb-c:special-operator-info progn (:forms (1 2)))
      :forms
      (((sb-c:self-evaluating-info 1 ()))
       ((sb-c:self-evaluating-info 2 ())))))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info if))
;; TODO
  (check-walk-cases
   '((if 1 2)
     ((sb-c:special-operator-info if (:condition 1 :then 2))
      :condition
      ((sb-c:self-evaluating-info 1 ()))
      :then
      ((sb-c:self-evaluating-info 2 ()))))
   '((if 1 2 3)
     ((sb-c:special-operator-info if (:condition 1 :then 2 :else 3))
      :condition
      ((sb-c:self-evaluating-info 1 ()))
      :then
      ((sb-c:self-evaluating-info 2 ()))
      :else
      ((sb-c:self-evaluating-info 3 ()))))))

;;; BLOCK and TAGBODY

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info block))
  ;; TODO
  (check-walk-cases
   '((block nil))
   '((block foo))
   '((block foo a b))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info return-from))
  ;; TODO
  (check-walk-cases
   '((return-from nil))
   '((return-from foo))
   '((return-from foo bla))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info tagbody))
  ;; TODO
  (check-walk-cases
   '((tagbody))
   '((tagbody nil))
   '((tagbody nil :foo))
   '((tagbody nil :foo :bar))
   '((tagbody nil (1+ a)))
   '((tagbody nil :foo (1+ a)))
   '((tagbody nil (1+ a) :foo))
   '((tagbody (1+ a)))
   '((tagbody (1+ a) (1+ b)))
   '((tagbody (1+ a) (1+ b) (1+ c)))
   '((tagbody :foo (1+ a) (1+ b)))
   '((tagbody (1+ a) :foo (1+ b)))
   '((tagbody (1+ a) (1+ b) :foo))))

;;;; Compiler-magic special forms
;;;; TODO test internal ones as well

(with-test (:name (sb-c:walk-forms :special-form eval-when))
  ;; TODO
  )

(with-test (:name (sb-c:walk-forms :special-form quote))

  (check-roundtrip-cases 'quote
                         '(quote 1)
                         '(quote x)
                         '(quote quote)))

(with-test (:name (sb-c:walk-forms :special-form function))

  (check-roundtrip-cases 'function
                         '(function foo)
                         '(function (setf foo))
                         '(function (lambda ()))
                         '(function (lambda (x)))
                         '(function (lambda (x) x))
                         '(function (lambda (x) x y))))

(with-test (:name (sb-c:walk-forms :special-form sb-c::global-function)) ; TODO correct package?
  ;; TODO
  )

;;;; SYMBOL-MACROLET, LET[*] and LOCALLY

(with-test (:name (sb-c:walk-forms :special-form symbol-macrolet))
  (check-walk-cases
   '((symbol-macrolet ())
     ((sb-c:special-operator-info symbol-macrolet
       (:names ()
        :expansions ()
        :declarations ()
        :body ()))
      :names () :expansions () :body nil))
   '((symbol-macrolet ((a 1) (c 1)) (declare (type boolean a)) a)
     ((sb-c:special-operator-info symbol-macrolet
       (:names (a c)
        :expansions (1 1)
        :declarations ((declare (type boolean a)))
        :body (a)))))))

(with-test (:name (sb-c:walk-forms :special-form let))
  (check-walk-cases
   '((let ())
     ((sb-c:special-operator-info let
       (:names ()
        :values ()
        :suppliedps ()
        :declarations ()
        :body nil))
      :names () :values () :body nil))
   '((let ((a 1) b (c 1)) (declare (type boolean a)) a)
     ((sb-c:special-operator-info let
       (:names (a b c)
        :values (1 nil 1)
        :suppliedps (t nil t)
        :declarations ((declare (type boolean a)))
        :body (a)))
      :names
      (((sb-c:variable-info a (:access :bind :where nil)))
       ((sb-c:variable-info b (:access :bind :where nil)))
       ((sb-c:variable-info c (:access :bind :where nil))))
      :values
      (((sb-c:self-evaluating-info 1))
       ((sb-c:variable-info nil
         (:access   :read
          :where    :global
          :constant t
          :type     t)))
       ((sb-c:self-evaluating-info 1)))
      :body
      (((sb-c:variable-info a
         (:access :read
          :where  :lexical))))))))

(with-test (:name (sb-c:walk-forms :special-form let*))
  (check-walk-cases 'let*
   '((let* ()))
   '((let* ((a 1) b (c 1)) (declare (type boolean a)) a))))

(with-test (:name (sb-c:walk-forms :special-form locally))
  (check-walk-cases 'locally
   '(locally)
   '(locally (declare (type integer a)))
   '(locally (declare (type integer a)) a)
   '(locally a)))
;; 
;; ;;;; MACROLET, FLET and LABELS
;;
;; (with-test (:name (sb-c:walk-forms :special-form macrolet))
;;
;;   (check-roundtrip-cases 'macrolet
;;                          '(macrolet ())
;;                          '(macrolet ((f ())))
;;                          '(macrolet ((f (a &rest b) (declare (type string a)) a)))))
;;
;; (with-test (:name (sb-c:walk-forms :special-form flet))
;;
;;   (check-roundtrip-cases 'flet
;;                          '(flet ())
;;                          '(flet ((f ())))
;;                          '(flet ((f (a &rest b) (declare (type string a)) a)))))
;;
;; (with-test (:name (sb-c:walk-forms :special-form labels))
;;
;;   (check-roundtrip-cases 'labels
;;                          '(labels ())
;;                          '(labels ((f ())))
;;                          '(labels ((f (a &rest b) (declare (type string a)) a)))))
;; 
;; ;;;; [TRULY-]THE
;;
;; (with-test (:name (sb-c:walk-forms :special-form the))
;;
;;   (check-roundtrip-cases 'the
;;                          '(the symbol 1)))
;;
;; (with-test (:name (sb-c:walk-forms :special-form truly-the))
;;
;;   (check-roundtrip-cases 'truly-the
;;                          '(truly-the symbol 1)))

;;;; SETQ

(define-symbol-macro walk-forms.setq.global
    (cdr foo))

(with-test (:name (sb-c:walk-forms :special-form setq))
  (check-walk-cases
   '((setq)
     ((sb-c:special-operator-info setq
       (:names () :value-forms ()))
      :names () :value-forms ()))
   '((setq a 1)
     ((sb-c:special-operator-info setq
       (:names (a) :value-forms (1)))
      :names
      (((sb-c:variable-info a (:where nil :access :write))))
      :value-forms
      (((sb-c:self-evaluating-info 1)))))
   '((setq a 1 b 2)
     ((sb-c:special-operator-info setq
       (:names (a b) :value-forms (1 2)))
      :names
      (((sb-c:variable-info a (:where nil :access :write)))
       ((sb-c:variable-info b (:where nil :access :write))))
      :value-forms
      (((sb-c:self-evaluating-info 1))
       ((sb-c:self-evaluating-info 2)))))
   ;; This is the interesting case: SETQ assignment to a symbol-macro.
   '((setq walk-forms.setq.global 1)
     ((sb-c:setq-with-symbol-macros-info setq
       (:names           (walk-forms.setq.global)
        :value-forms     (1)
        :symbol-macro-ps :ignore ; TODO should be (<generalized true>)
        :expander        :ignore
        :arguments       (walk-forms.setq.global 1)))
      ((sb-c:macro-info setf
        (:expander  :ignore
         :arguments ((cdr foo) 1)
         :where     :global))
       ((sb-c:application-info sb-kernel:%rplacd
         (:where :global
          :arguments (foo 1)))
        :arguments
        (((sb-c:variable-info foo (:where nil :access :read)))
         ((sb-c:self-evaluating-info 1 ())))))))))
;; 
;; ;;;; THROW, CATCH and UNWIND-PROTECT
;;
;; (with-test (:name (sb-c:walk-forms :special-form throw))
;;
;;   (check-roundtrip-cases 'throw
;;                          '(throw 'foo 1)
;;                          '(throw (get-tag) 2)))
;;
;; (with-test (:name (sb-c:walk-forms :special-form sb-c::%within-cleanup))
;;
;;   (check-roundtrip-cases 'sb-c::%within-cleanup
;;                          '(sb-c::%within-cleanup :kind (mess-up) (foo) bar)))
;;
;; (with-test (:name (sb-c:walk-forms :special-form sb-c::%escape-fun))
;;
;;   (check-roundtrip-cases 'sb-c::%escape-fun
;;                          '(sb-c::%escape-fun foo)))
;;
;; (with-test (:name (sb-c:walk-forms :special-form sb-c::%cleanup-fun))
;;
;;   (check-roundtrip-cases 'sb-c::%cleanup-fun
;;                          '(sb-c::%cleanup-fun (setf foo))))
;;
;; (with-test (:name (sb-c:walk-forms :special-form catch))
;;
;;   (check-roundtrip-cases 'catch
;;                          '(catch 'foo)
;;                          '(catch 'foo 1)
;;                          '(catch 'foo 1 2)
;;                          '(catch (get-tag))))
;;
;; (with-test (:name (sb-c:walk-forms :special-form unwind-protect))
;;
;;   (check-roundtrip-cases 'unwind-protect
;;                          '(unwind-protect foo)
;;                          '(unwind-protect foo bar)
;;                          '(unwind-protect foo bar baz)))
;; 
;; ;;;; multiple-value stuff
;;
;; (with-test (:name (sb-c:walk-forms :special-form multiple-value-call))
;;
;;   (check-roundtrip-cases 'multiple-value-call
;;                          '(multiple-value-call fun)
;;                          '(multiple-value-call fun 1)))
;;
;; (with-test (:name (sb-c:walk-forms :special-form multiple-value-prog1))
;;
;;   (check-roundtrip-cases 'multiple-value-prog1
;;                          '(multiple-value-prog1 1)
;;                          '(multiple-value-prog1 1 2)
;;                          '(multiple-value-prog1 1 2 3)))
;; 
;; ;;;;

;;;; Kind :APPLICATION, LAMBDA variant

(with-test (:name (sb-c:walk-forms :application lambda))
  (check-walk-cases
   '(((lambda ()))
     ((sb-c:lambda-application-info lambda
       (:lambda-list () :body () :arguments ()))
      :body nil :arguments nil))
   '(((lambda ()) 1)
     ((sb-c:lambda-application-info lambda
       (:lambda-list () :body () :arguments (1)))
      :body nil
      :arguments (((sb-c:self-evaluating-info 1)))))
   '(((lambda (x)))
     ((sb-c:lambda-application-info lambda
       (:lambda-list (x) :body () :arguments ()))
      :body nil :arguments nil))
   '(((lambda () x))
     ((sb-c:lambda-application-info lambda
       (:lambda-list () :body (x) :arguments ()))
      :body
      (((sb-c:variable-info x (:access :read :where nil))))
      :arguments nil))
   ;; TODO case with lexical variable in the body?
   ))

;;;; Kind :APPLICATION, named variant

(with-test (:name (sb-c:walk-forms :application :named :global))
  (check-walk-cases
   '((+)
     ((sb-c:named-application-info + (:arguments () :where :global))
      :arguments nil))
   '((+ 1)
     ((sb-c:named-application-info + (:arguments (1) :where :global))
      :arguments
      (((sb-c:self-evaluating-info 1)))))))

(with-test (:name (sb-c:walk-forms :application :named :lexical))
  (let ((env (sb-c::coerce-to-lexenv nil)))
    (sb-c:note-lexical-function 'lexical '(x) nil '() '((1+ x)) env)
    (check-walk-cases/environment
     env
     '((lexical)
       ((sb-c:named-application-info lexical
         (:arguments ()
          :where :lexical
          :lambda-list (x)
          :documentation nil
          :declarations ()
          :body ((1+ x))))
        :arguments nil))
     '((lexical 1)
       ((sb-c:named-application-info lexical
         (:arguments (1)
          :where :lexical
          :lambda-list (x)
          :documentation nil
          :declarations ()
          :body ((1+ x))))
        :arguments
        (((sb-c:self-evaluating-info 1))))))))

(with-test (:name (sb-c:walk-forms :application :named :undefined))
  (check-walk-cases
   '((undefined)
     ((sb-c:named-application-info undefined (:arguments () :where nil))
      :arguments nil))
   '((undefined 1)
     ((sb-c:named-application-info undefined (:arguments (1) :where nil))
      :arguments
      (((sb-c:self-evaluating-info 1)))))))

;;;; Kind :MACRO

(with-test (:name (sb-c:walk-forms :macro :global))
  (check-walk-cases
   '((return)
     ((sb-c:macro-info return (:arguments () :expander :ignore :where :global))
      ((sb-c:special-operator-info return-from (:name nil)))))
   '((return *)
     ((sb-c:macro-info return (:arguments (*) :expander :ignore :where :global))
      ((sb-c:special-operator-info return-from (:name nil :value *))
       :value ((:variable * (:where :global :type t))))))))

(with-test (:name (sb-c:walk-forms :macro :lexical))
  ;; TODO
  )

;;;; Kind :SYMBOL-MACRO

(define-symbol-macro walk-forms.symbol-macro.global
    (1+ 1))

(with-test (:name (sb-c:walk-forms :symbol-macro :global))
  (check-walk-cases
   '(walk-forms.symbol-macro.global
     ((sb-c:symbol-macro-info walk-forms.symbol-macro.global
       (:where :global :expander :ignore))
      ((sb-c:named-application-info 1+ (:where :global :arguments (1)))
       :arguments
       (((sb-c:self-evaluating-info 1))))))))

(with-test (:name (sb-c:walk-forms :symbol-macro :lexical))
  ;; TODO
  )
;; 
;; ;;;; Kind :COMPILER-MACRO
;;
;; (with-test (:name (sb-c:walk-forms :compiler-macro))
;;   ;; TODO
;;   )

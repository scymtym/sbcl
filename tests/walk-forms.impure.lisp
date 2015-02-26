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
(use-package '#:assertoid)

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

(defun check-component-list (form alist expected-alist check-value)
  (let ((diff1 (set-difference alist expected-alist
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
       (funcall check-value key value))))

(defun check-component-list-vs-expected (form alist expected-alist)
  (check-component-list
   form alist expected-alist
   (lambda (key value)
     (let ((expected-value (cdr (assoc key expected-alist))))
       (case expected-value
         (:ignore)
         (t
          (unless (equal value expected-value)
            (error "~@<In ~S: unexpected value of ~S property; actual: ~
                    ~S, expected: ~S~@:>"
                   form key value expected-value))))))))

(defun check-component-list-vs-info (form alist info)
  (let ((components (loop :for component :in (sb-c::operator-info-components info)
                       :collect (cons (sb-c::operator-component-name component)
                                      component))))
    (check-component-list
     form alist components
     (lambda (key value)
       (let ((component (cdr (assoc key components))))
         (assert (typep value (sb-c::operator-component-type component)))))))) ; TODO error message

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
                 (assert (equal name expected-name)) ; name can be number, string, etc => EQUAL
                 (let ((alist (plist-alist plist))
                       (expected-alist (plist-alist expected-plist)))
                   (check-component-list-vs-expected form alist expected-alist)
                   #+TODO (check-component-list-vs-info form alist kind))
                 ;; Recurse into subforms.
                 ;; TODO same diff algorithm as for properties?
                 (cond
                   ((subtypep expected-kind 'sb-c:macroid-info)
                    (check-form subforms (first expected-subforms))
                    (setf expected-subforms nil))
                   (t
                    (loop :for (key subform) :on subforms :by #'cddr :do
                       (let ((expected-subform
                              (getf expected-subforms key :missing))
                             (component
                              (find key (sb-c::operator-info-components kind)
                                    :key #'sb-c::operator-component-name)))
                         (case expected-subform
                           (:missing
                            (error "~@<In ~S: unexpected subform ~S ~S.~@:>"
                                   form key subform))
                           (t
                            (case (sb-c::operator-component-cardinality component)
                              (1 (check-form subform expected-subform))
                              (* (mapc #'check-form subform expected-subform))))))
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
                (assert (typep info '(cons ))) ; TODO
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
   '(1    ((sb-c:self-evaluating-info 1 ())))
   '("1"  ((sb-c:self-evaluating-info "1" ())))
   '(:foo ((sb-c:self-evaluating-info :foo ())))))


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
   '((block nil)
     ((sb-c:special-operator-info block (:name nil))))
   '((block foo)
     ((sb-c:special-operator-info block (:name foo))))
   '((block foo 1)
     ((sb-c:special-operator-info block (:name foo :forms (1)))
      :forms
      (((sb-c:self-evaluating-info 1 ())))))
   '((block foo 1 2)
     ((sb-c:special-operator-info block (:name foo :forms (1 2)))
      :forms
      (((sb-c:self-evaluating-info 1 ()))
       ((sb-c:self-evaluating-info 2 ())))))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info return-from))
  ;; TODO
  (check-walk-cases
   '((return-from nil)
     ((sb-c:special-operator-info return-from (:name nil))))
   '((return-from foo)
     ((sb-c:special-operator-info return-from (:name foo))))
   '((return-from foo 1)
     ((sb-c:special-operator-info return-from (:name foo :value 1))
      :value
      ((sb-c:self-evaluating-info 1 ()))))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info tagbody))
  ;; TODO
  (check-walk-cases
   '((tagbody)
     ((sb-c:special-operator-info tagbody (:tags () :segments ((progn))))
      :segments
      ()))
   '((tagbody nil)
     ((sb-c:special-operator-info tagbody (:tags     (nil)
                                           :segments ((progn) (progn))))
      :segments
      ()))
   '((tagbody nil :foo)
     ((sb-c:special-operator-info tagbody (:tags     (nil :foo)
                                           :segments ((progn) (progn) (progn))))
      :segments
      ()))
   '((tagbody nil :foo :bar)
     ((sb-c:special-operator-info tagbody (:tags     (nil :foo :bar)
                                           :segments ((progn) (progn) (progn) (progn))))
      :segments
      ()))
   '((tagbody nil '1)
     ((sb-c:special-operator-info tagbody (:tags     (nil)
                                           :segments ((progn) (progn '1))))
      :segments
      ()))
   '((tagbody nil :foo '1)
     ((sb-c:special-operator-info tagbody (:tags     (nil :foo)
                                           :segments ((progn) (progn) (progn '1))))
      :segments
      ()))
   '((tagbody nil '1 :foo)
     ((sb-c:special-operator-info tagbody (:tags ()
                                           :segments ((progn) (progn '1) (progn))))
      :segments
      ()))
   '((tagbody '1)
     ((sb-c:special-operator-info tagbody (:tags () :segments ((progn))))
      :segments
      ()))
   '((tagbody '1 '2)
     ((sb-c:special-operator-info tagbody (:tags () :segments ((progn))))
      :segments
      ()))
   '((tagbody '1 '2 '3)
     ((sb-c:special-operator-info tagbody (:tags () :segments ((progn))))
      :segments
      ()))
   '((tagbody :foo '1 '2)
     ((sb-c:special-operator-info tagbody (:tags () :segments ((progn))))
      :segments
      ()))
   '((tagbody '1 :foo '2)
     ((sb-c:special-operator-info tagbody (:tags () :segments ((progn))))
      :segments
      ()))
   '((tagbody '1 '2 :foo)
     ((sb-c:special-operator-info tagbody (:tags () :segments ((progn))))
      :segments
      ()))))

;;;; Compiler-magic special forms
;;;; TODO test internal ones as well

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info eval-when))
  ;; TODO
  )

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info quote))
  ;; TODO
  (check-walk-cases
   '((quote 1)
     ((sb-c:special-operator-info quote (:thing 1))))
   '((quote x)
     ((sb-c:special-operator-info quote (:thing x))))
   '((quote quote)
     ((sb-c:special-operator-info quote (:thing quote))))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info function))
  ;; TODO
  (check-walk-cases
   '((function foo)
     ((sb-c:special-operator-info function (:name foo))))
   '((function (setf foo))
     ((sb-c:special-operator-info function (:name (setf foo)))))
   '((function (lambda ()))
     ((sb-c:special-operator-info function ())))
   '((function (lambda (x)))
     ((sb-c:special-operator-info function ())))
   '((function (lambda (x) x))
     ((sb-c:special-operator-info function ())))
   '((function (lambda (x) x y))
     ((sb-c:special-operator-info function ())))))

(with-test (:name (sb-c:walk-forms :special-form sb-c::global-function)) ; TODO correct package?
  ;; TODO
  )

;;;; SYMBOL-MACROLET, LET[*], LOCALLY and PROGV

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info symbol-macrolet))
  (check-walk-cases ; TODO use /environment variant?
   '((symbol-macrolet ())
     ((sb-c:special-operator-info symbol-macrolet
       (:names ()
        :expansions ()
        :declarations ()
        :body ()))
      :names () :expansions () :body nil))
   '((symbol-macrolet ((a 1) (c 1)) (declare (type boolean a)) a)
     ((sb-c:special-operator-info symbol-macrolet
       (:names        (a c)
        :expansions   (1 1)
        :declarations ((declare (type boolean a)))
        :body         (a)))
      :names
      (((sb-c:symbol-macro-info a (:expander :ignore :where :lexical)) ; TODO unfinished
        ))
      :expansions
      (((sb-c:self-evaluating-info 1 ())
        (sb-c:self-evaluating-info 1 ())))
      :body
      (((sb-c:variable-info a ())))))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info let))
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
       (:names        (a b   c)
        :values       (1 nil 1)
        :suppliedps   (t nil t)
        :declarations ((declare (type boolean a)))
        :body         (a)))
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
          :where  :lexical))))))
   #+TODO '((let ((a 1) (b a))))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info let*))
  (check-walk-cases
   '((let* ())
     ((sb-c:special-operator-info let*
       (:names        ()
        :values       ()
        :suppliedps   ()
        :declarations ()
        :body         nil))))
   '((let* ((a 1) b (c 1)) (declare (type boolean a)) a)
     ((sb-c:special-operator-info let*
       (:names        (a b   c)
        :values       (1 nil 1)
        :suppliedps   (t nil t)
        :declarations ((declare (type boolean a)))
        :body         (a)))))
   #+TODO '((let* ((a 1) (b a))))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info locally))
  (check-walk-cases
   '((locally)
     ((sb-c:special-operator-info locally ())))
   '((locally (declare (type integer a)))
     ((sb-c:special-operator-info locally ())))
   '((locally (declare (type integer a)) a)
     ((sb-c:special-operator-info locally ())))
   '((locally a)
     ((sb-c:special-operator-info locally ())))))

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info progv))
  (check-walk-cases
   '((progv () ()))
   '((progv '(a) '(1)))
   '((progv (foo) '(1)))
   '((progv () () 1))
   '((progv () () 1 2))))

;;;; MACROLET, FLET and LABELS

(with-test (:name (sb-c:walk-forms sb-c:special-operator-info macrolet))
  (check-walk-cases ; TODO use the /environment variant?
   '((macrolet ())
     ((sb-c:special-operator-info macrolet
       (:names () ()))))
   '((macrolet ((f ())))
     ((sb-c:special-operator-info macrolet
       (:names (f) ()))))
   '((macrolet ((f (a &rest b) (declare (type string a)) a)))
     ((sb-c:special-operator-info macrolet
       (:names (f) ()))))))
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
         (:where     :global
          :arguments (foo 1)))
        :arguments
        (((sb-c:variable-info foo (:where nil :access :read)))
         ((sb-c:self-evaluating-info 1 ())))))))))

;;;; THROW, CATCH and UNWIND-PROTECT

(with-test (:name (sb-c:walk-forms :special-form throw))
  (check-walk-cases
   '((throw 'foo 1)
     ((sb-c:special-operator-info throw (:tag 'foo :result-form 1))
      :tag
      ((sb-c:special-operator-info quote (:thing foo)))
      :result-form
      ((sb-c:self-evaluating-info 1))))
   '((throw (get-tag) 2)
     ((sb-c:special-operator-info throw (:tag (get-tag) :result-form 2))
      :tag
      ((sb-c:named-application-info get-tag (:arguments () :where nil))
       :arguments
       ())
      :result-form
      ((sb-c:self-evaluating-info 2))))))

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

;;;; multiple-value stuff

(with-test (:name (sb-c:walk-forms :special-form multiple-value-call))
  (check-walk-cases
   '((multiple-value-call fun)
     ((sb-c:special-operator-info multiple-value-call (:function-form fun))
      :function-form
      ((sb-c:variable-info fun (:where nil :access :read)))))
   '((multiple-value-call fun 1)
     ((sb-c:special-operator-info multiple-value-call (:function-form fun :args (1)))
      :function-form
      ((sb-c:variable-info fun (:where nil :access :read)))
      :args
      (((sb-c:self-evaluating-info 1)))))))

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
   ;; TODO declaration
   '(((lambda () "doc" 1))
     ((sb-c:lambda-application-info lambda
       (:lambda-list () :documentation "doc" :body (1) :arguments ()))
      :body
      (((sb-c:self-evaluating-info 1)))))
   ;; TODO case with lexical variable in the body?
   ))

;;;; Kind :APPLICATION, named variant

(with-test (:name (sb-c:walk-forms :application :named :global))
  (check-walk-cases
   '((+)
     ((sb-c:named-application-info + (:arguments () :where :global))
      :arguments
      ()))
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
         (:arguments     ()
          :where         :lexical
          :lambda-list   (x)
          :documentation nil
          :declarations  ()
          :body          ((1+ x))))
        :arguments
        ()))
     '((lexical 1)
       ((sb-c:named-application-info lexical
         (:arguments     (1)
          :where         :lexical
          :lambda-list   (x)
          :documentation nil
          :declarations  ()
          :body          ((1+ x))))
        :arguments
        (((sb-c:self-evaluating-info 1))))))))

(with-test (:name (sb-c:walk-forms :application :named :undefined))
  (check-walk-cases
   '((undefined)
     ((sb-c:named-application-info undefined (:arguments () :where nil))
      :arguments
      ()))
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
       :value
       ((:variable * (:where :global :type t))))))))

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

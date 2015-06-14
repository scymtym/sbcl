;;;; Test special operator (un)parsing.

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

(load "assertoid.lisp")
(cl:use-package #:assertoid)

;;; Utilities

(defun check-roundtrip (special-operator form)
  (let* ((info (sb-c::find-special-operator-info special-operator))
         (parser (sb-c::special-operator-info-parser info))
         (unparser (sb-c::special-operator-info-unparser info)))
    (assert (equal form (funcall parser unparser form)))))

(defun check-roundtrip-cases (special-operator &rest forms)
  (dolist (form forms) (check-roundtrip special-operator form)))

(defun check-error (special-operator form expected-error)
  (let* ((info (sb-c::find-special-operator-info special-operator))
         (parser (sb-c::special-operator-info-parser info)))
    (assert (typep (nth-value 1 (ignore-errors (funcall parser #'list form))) expected-error)))) ; TODO

(defun check-error-cases (special-operator &rest specs)
  (loop :for (form expected-error) :in specs :do
     (check-error special-operator form expected-error)))

;;;; Special operators for control

(with-test (:name (:special-operator progn))
  ;; TODO
  )

(with-test (:name (:special-operator if))
  (check-error-cases 'if
    '((if) sb-kernel::arg-count-error)
    '((if foo) sb-kernel::arg-count-error)
    '((if foo bar baz fez) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'if
    '(if foo bar)
    '(if foo bar baz)))

;;; BLOCK and TAGBODY

(with-test (:name (:special-operator block))
  #+TODO (parse-block-special-operator #'unparse-block-special-operator '(block foo a b)))

(with-test (:name (:special-operator return-from))
  #+TODO (apply #'unparse-return-from-special-operator
        (parse-return-from-special-operator
         (lambda (&rest args) (print args))
         '(return-from foo bla))))

(with-test (:name (:special-operator tagbody))
  (check-error-cases 'tagbody
    '((tagbody nil nil) program-error))
  (check-roundtrip-cases 'tagbody
    '(tagbody)
    '(tagbody nil)
    '(tagbody nil :foo)
    '(tagbody nil :foo :bar)
    '(tagbody nil (1+ a))
    '(tagbody nil :foo (1+ a))
    '(tagbody nil (1+ a) :foo)
    '(tagbody (1+ a))
    '(tagbody (1+ a) (1+ b))
    '(tagbody (1+ a) (1+ b) (1+ c))
    '(tagbody :foo (1+ a) (1+ b))
    '(tagbody (1+ a) :foo (1+ b))
    '(tagbody (1+ a) (1+ b) :foo)))

;;;; Compiler-magic special forms
;;;; TODO test internal ones as well

(with-test (:name (:special-operator eval-when))
  ;; TODO
  )

(with-test (:name (:special-operator quote))
  (check-error-cases 'quote
    '((quote) sb-kernel::arg-count-error)
    '((quote x y) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'quote
    '(quote 1)
    '(quote x)
    '(quote quote)))

(with-test (:name (:special-operator function))
  (check-error-cases 'function
    '((function) sb-kernel::arg-count-error)
    '((function 1) program-error)
    '((function x y) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'function
    '(function foo)
    '(function (setf foo))
    '(function (lambda ()))
    '(function (lambda (x)))
    '(function (lambda (x) x))
    '(function (lambda (x) x y))))

(with-test (:name (:special-operator sb-c::global-function)) ; TODO correct package?
  ;; TODO
  )

;;;; SYMBOL-MACROLET, LET[*], LOCALLY and PROGV

(with-test (:name (:special-operator symbol-macrolet))
  (check-error-cases 'symbol-macrolet
    '((symbol-macrolet (foo)) program-error)
    '((symbol-macrolet ((foo))) program-error)
    '((symbol-macrolet ((:bla 1))) program-error))
  (check-roundtrip-cases 'symbol-macrolet
    '(symbol-macrolet ())
    '(symbol-macrolet ((a 1) (c 1)) (declare (type boolean a)) a)))

(with-test (:name (:special-operator let))
  (check-error-cases 'let
    '((let) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'let
    '(let ())
    '(let ((a 1) b (c 1)) (declare (type boolean a)) a)))

(with-test (:name (:special-operator let*))
  (check-error-cases 'let*
    '((let*) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'let*
    '(let* ())
    '(let* ((a 1) b (c 1)) (declare (type boolean a)) a)))

(with-test (:name (:special-operator locally))
  (check-roundtrip-cases 'locally
    '(locally)
    '(locally (declare (type integer a)))
    '(locally (declare (type integer a)) a)
    '(locally a)))

(with-test (:name (:special-operator progv))
  (check-roundtrip-cases 'progv
    '(progv () ())
    '(progv '(a) '(1))
    '(progv (foo) '(1))
    '(progv () () 1)
    '(progv () () 1 2)))

;;;; MACROLET, FLET and LABELS

(with-test (:name (:special-operator macrolet))
  (check-error-cases 'macrolet
    '((macrolet) sb-kernel::arg-count-error)
    '((macrolet ((f))) program-error)
    '((macrolet ((f 1))) program-error))
  (check-roundtrip-cases 'macrolet
    '(macrolet ())
    '(macrolet ((f ())))
    '(macrolet ((f (a &rest b) (declare (type string a)) a)))))

(with-test (:name (:special-operator flet))
  (check-error-cases 'flet
    '((flet) sb-kernel::arg-count-error)
    '((flet ((f 1))) program-error))
  (check-roundtrip-cases 'flet
    '(flet ())
    '(flet ((f ())))
    '(flet ((f (a &rest b) (declare (type string a)) a)))))

(with-test (:name (:special-operator labels))
  (check-error-cases 'labels
    '((labels) sb-kernel::arg-count-error)
    '((labels ((f 1))) program-error))
  (check-roundtrip-cases 'labels
    '(labels ())
    '(labels ((f ())))
    '(labels ((f (a &rest b) (declare (type string a)) a)))))

;;;; [TRULY-]THE

(with-test (:name (:special-operator the))
  (check-error-cases 'the
    '((the) sb-kernel::arg-count-error)
    '((the symbol) sb-kernel::arg-count-error)
    '((the symbol value extra) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'the
    '(the symbol 1)))

(with-test (:name (:special-operator truly-the))
  (check-error-cases 'truly-the
    '((truly-the) sb-kernel::arg-count-error)
    '((truly-the symbol) sb-kernel::arg-count-error)
    '((truly-the symbol value extra) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'truly-the
    '(truly-the symbol 1)))

;;;; SETQ

(define-symbol-macro special-operators.setq.global
    (cdr foo))

(with-test (:name (:special-operator setq))
  (check-error-cases 'setq
    '((setq a) program-error)
    '((setq a 1 b) program-error)
    '((setq (1+ a) 1) program-error))
  (check-roundtrip-cases 'setq
    '(setq)
    '(setq a 1)
    '(setq special-operators.setq.global 1)))

;;;; THROW, CATCH and UNWIND-PROTECT

(with-test (:name (:special-operator throw))
  (check-error-cases 'throw
    '((throw) sb-kernel::arg-count-error)
    '((throw 'foo) sb-kernel::arg-count-error)
    '((throw 'foo 1 extra) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'throw
    '(throw 'foo 1)
    '(throw (get-tag) 2)))

(with-test (:name (:special-operator sb-c::%within-cleanup))
  (check-error-cases 'sb-c::%within-cleanup
    '((sb-c::%within-cleanup) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'sb-c::%within-cleanup
    '(sb-c::%within-cleanup :kind (mess-up) (foo) bar)))

(with-test (:name (:special-operator sb-c::%escape-fun))
  (check-error-cases 'sb-c::%escape-fun
    '((sb-c::%escape-fun) sb-kernel::arg-count-error)
    '((sb-c::%escape-fun foo bar) sb-kernel::arg-count-error)
    '((sb-c::%escape-fun 1) program-error)
    '((sb-c::%escape-fun (setf foo)) program-error))
  (check-roundtrip-cases 'sb-c::%escape-fun
    '(sb-c::%escape-fun foo)))

(with-test (:name (:special-operator sb-c::%cleanup-fun))
  (check-error-cases 'sb-c::%cleanup-fun
    '((sb-c::%cleanup-fun) sb-kernel::arg-count-error)
    '((sb-c::%cleanup-fun x y) sb-kernel::arg-count-error)
    '((sb-c::%cleanup-fun 1) program-error)
    '((sb-c::%cleanup-fun (setf foo bar)) program-error))
  (check-roundtrip-cases 'sb-c::%cleanup-fun
    '(sb-c::%cleanup-fun (setf foo))))

(with-test (:name (:special-operator catch))
  (check-error-cases 'catch
    '((catch) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'catch
    '(catch 'foo)
    '(catch 'foo 1)
    '(catch 'foo 1 2)
    '(catch (get-tag))))

(with-test (:name (:special-operator unwind-protect))
  (check-error-cases 'unwind-protect
    '((unwind-protect) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'unwind-protect
    '(unwind-protect foo)
    '(unwind-protect foo bar)
    '(unwind-protect foo bar baz)))

;;;; multiple-value stuff

(with-test (:name (:special-operator multiple-value-call))
  (check-error-cases 'multiple-value-call
    '((multiple-value-call) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'multiple-value-call
    '(multiple-value-call fun)
    '(multiple-value-call fun 1)))

(with-test (:name (:special-operator multiple-value-prog1))
  (check-error-cases 'multiple-value-prog1
    '((multiple-value-prog1) sb-kernel::arg-count-error))
  (check-roundtrip-cases 'multiple-value-prog1
    '(multiple-value-prog1 1)
    '(multiple-value-prog1 1 2)
    '(multiple-value-prog1 1 2 3)))

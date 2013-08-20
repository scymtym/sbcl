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
(use-package "ASSERTOID")
(use-package "TEST-UTIL")

;;; Tests for recursive types
;;;
;;; The following tests are concerned with valid and invalid recursive
;;; types.
;;;
;;; See X3J13 Issue RECURSIVE-DEFTYPE (at the time of writing
;;; available at http://clhs.lisp.se/Issues/iss291_w.htm) for some
;;; background.

;;; Recursive types for the following tests

(deftype recursive-union-type.1 ()
  '(or (eql :a) recursive-union-type.1))

(deftype recursive-union-type.2 ()
  '(or (eql :a) (or (eql :b) recursive-union-type.2)))

(deftype recursive-intersection-type ()
  '(and (eql :a) recursive-intesection-type))

(deftype proper-list (&optional (element-type t))
  `(or null (cons ,element-type (proper-list ,element-type))))

;;; Test parsing and unparsing of recursive types

(let ((*print-circle* t))
  (sb-kernel:specifier-type '#1=(cons integer #1#)))

(sb-kernel:specifier-type 'recursive-union-type.1)

(with-test (:name :specifier-type :recursive-type)

  (sb-kernel:specifier-type '#1=(or null (cons integer #1#)))

  (sb-kernel:specifier-type '(proper-list (or (eql :a) (eql :b) integer))))

(with-test (:name :type-specifier :recursive-type)

  (sb-kernel:type-specifier (sb-kernel:specifier-type '#1=(or null (cons integer #1#))))

  (sb-kernel:type-specifier (sb-kernel:specifier-type '(proper-list (or (eql :a) (eql :b) integer)))))

;;; Test *invalid* recursive types RECURSIVE-UNION-TYPE*

(with-test (:name :recursive-union-type)
  (macrolet ((test (object type)
               `(assert (raises-error?
                         (typep ,object ,type) sb-int:type-parse-error))
               ;; Repeat once because SPECIFIER-TYPE contains a cache.
               #+NO (assert (raises-error?
                             (typep ,object ,type) sb-int:type-parse-error))))
    (test t 'recursive-union-type.1)
    (test t 'recursive-union-type.2)))

;;; Test *invalid* recursive type RECURSIVE-INTERSECTION-TYPE

(with-test (:name :recursive-intersection-type)
  (assert (raises-error? (typep t 'recursive-intersection-type) type-parse-error)))

;;; Test recursive type PROPER-LIST

(with-test (:name (typep :resursive-type :compile))
  (defun f ()
    (let ((l (make-list 10 :initial-element 1)))
      (time (typep l '(proper-list integer))))))

(with-test (:name (typep :recursive-type :proper-list))
  (docases ((element-type object expected)
            (assert (eq expected
                        (typep object `(proper-list ,element-type)))))
    ;;element-type          object    expected
    '(t                     1         nil)
    '(t                     "foo"     nil)
    '(t                     ()        t)
    '(t                     (:a)      t)
    '(t                     (1)       t)

    '(integer               1         nil)
    '(integer               :foo      nil)
    '(integer               (:foo)    nil)
    '(integer               ()        t)
    '(integer               (1)       t)
    `(integer               ,(make-list 1000 :initial-element 1) t)

    '((or (eql :a) integer) 1         nil)
    '((or (eql :a) integer) :foo      nil)
    '((or (eql :a) integer) (:foo)    nil)
    '((or (eql :a) integer) (:a :foo) nil)
    '((or (eql :a) integer) ()        t)
    '((or (eql :a) integer) (:a)      t)
    '((or (eql :a) integer) (:a :a)   t)
    '((or (eql :a) integer) (1)       t)
    '((or (eql :a) integer) (1 1)     t)
    '((or (eql :a) integer) (:a 1)    t)
    '((or (eql :a) integer) (1 :a)    t)))

;;; Test recursive *expander* in type LIST-OF-LENGTH

(deftype list-of-length (length)
  (if (= 0 length)
      'null
      `(cons t (list-of-length ,(- length 1)))))

;;;

(deftype foo () '(cons foo (or null foo)))

(let ((type (sb-kernel:specifier-type 'foo)))
  (describe type))

(deftype bar () '(cons (eql :a) (or null bar)))

(let ((type (sb-kernel:specifier-type 'bar)))
  (describe type))


;;

(deftype foo () '(cons foo (or null foo)))

(sb-kernel:specifier-type 'foo)

(deftype bar () '(cons (eql :a) (or null bar)))

(typep '(:a :a :a) 'bar)

(typexpand 'list)

(sb-kernel:recursive-type-p (sb-kernel:specifier-type 'list))
(sb-kernel:recursive-type-p (sb-kernel:specifier-type '(not bar)))

(let ((*print-circle* t))
  (princ-to-string (sb-kernel:type-specifier (sb-kernel:specifier-type 'bar))))

(let ((type (sb-kernel:specifier-type 'bar)))
  (describe type)
  type)

(deftype fez () '(or (eql :b) bar))

(typep '(:a :a :a) 'fez)
(typep :b 'fez)

(let ((type (sb-kernel:specifier-type 'fez)))
  (describe type)
  type)


(defun f ()
  (let ((l (make-list 10 :initial-element 2)))
    (declare (type (proper-list integer) l))
    (char (cadddr l) 0)
    #+no (char (car l) 0)))

(defun g (x)
  (string= "a" (the (or null integer) x)))

(let ((c (sb-kernel:specifier-type '(proper-list integer)))
      (*print-circle* t))
  (princ-to-string (sb-kernel:type-specifier c)))

(sb-kernel:type= (sb-kernel:specifier-type '(proper-list integer))
                 (sb-kernel:specifier-type '(proper-list integer)))

(sb-c::source-transform-typep 'object '(proper-list integer))

(defun h (x)
  (LABELS ((NAME1725 (OBJECT*-1726)
             (LET ((N-OBJ1727 OBJECT*-1726))
               (OR (EQL N-OBJ1727 'NIL)
                   (LABELS ((NAME1728 (OBJECT*-1729)
                              (LET ((N-OBJ1730 OBJECT*-1729))
                                (AND (CONSP N-OBJ1730)
                                     (TYPEP (CAR N-OBJ1730) 'INTEGER)
                                     (NAME1725 (CDR N-OBJ1730))))))
                     (DECLARE (DYNAMIC-EXTENT (FUNCTION NAME1728)))
                     (NAME1728 N-OBJ1727))))))
    (DECLARE (DYNAMIC-EXTENT (FUNCTION NAME1725)))
    (name1725 x)))
(proclaim '(optimize (speed 3) (debug 1) (safety 1)))

(defun f (a b c d e f)
  (LABELS ((NAME1725 (OBJECT*-1726)
             (LET ((N-OBJ1727 OBJECT*-1726))
               (OR (EQL N-OBJ1727 'NIL)
                   (LABELS ((NAME1728 (OBJECT*-1729)
                              (LET ((N-OBJ1730 OBJECT*-1729))
                                (AND (CONSP N-OBJ1730)
                                     (TYPEP (CAR N-OBJ1730) 'INTEGER)
                                     (NAME1725 (CDR N-OBJ1730))))))
                     (DECLARE (DYNAMIC-EXTENT (FUNCTION NAME1728)))
                     (NAME1728 N-OBJ1727))))))
    (DECLARE (DYNAMIC-EXTENT (FUNCTION NAME1725)))
    (time (loop repeat 100000
                while (or (NAME1725 a)
                           (NAME1725 b)
                           (NAME1725 c)
                           (NAME1725 d)
                           (NAME1725 e)
                           (NAME1725 f))))))

(defun h2 ()
  (values (sb-kernel:%typep nil '(proper-list integer))
          (sb-kernel:%typep '(1) '(proper-list integer))
          (sb-kernel:%typep '(1 2) '(proper-list integer))
          (sb-kernel:%typep '(1 2 3) '(proper-list integer))
          (sb-kernel:%typep '(1 2 3 5.0) '(proper-list integer))
          (sb-kernel:%typep '(1.0 2 3 5.0) '(proper-list integer))))

(defun g (a b c d e f)
  (time (loop repeat 100000
              while (or (sb-kernel:%typep a '(proper-list integer))
                        (sb-kernel:%typep b '(proper-list integer))
                        (sb-kernel:%typep c '(proper-list integer))
                        (sb-kernel:%typep d '(proper-list integer))
                        (sb-kernel:%typep e '(proper-list integer))
                        (sb-kernel:%typep f '(proper-list integer))))))

;;; Test cases from X3J13 Issue RECURSIVE-DEFTYPE

;; 1

(defun returns-t (x) x)
(deftype foo () '(or (satisfies returns-t) foo))

;; This is an error because in (TYPEP X 'FOO), FOO cannot, in general,
;; be fully expanded because the expansion does not terminate.
#+TODO-shoud-signal (sb-ext:typexpand-all 'foo)

;; 2

(deftype bar (&key (fast nil))
  (if fast
      `(satisfies bar-fast)
      `(and symbol (bar :fast t))))

;; This is well-defined because in (TYPEP X 'FOO), FOO can, in
;; general, be fully expanded without error because the recursion
;; always terminates.
(equal '(and symbol (satisfies bar-fast)) (sb-ext:typexpand-all 'bar))
(equal '(satisfies bar-fast) (sb-ext:typexpand-all '(bar :fast t)))

;; 5 Type specifiers involving explicit circularities, such as

;; a.

#+TODO-should-signal (typep 3 '(and #1=(t . #1#)))
;; would be explicitly undefined


;; except for cases like
;; b.

#+TODO-should-signal (typep 3 '(eql #1=(t . #1#)))
;; which would still be well-defined.

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

;; Utilities

(defmacro twice (&body body)
  `(progn ,@body ,@body))

;;; Since this file makes excessive use of circular structures, we do
;;; the following as a safety measure:

(setf *print-circle* nil
      *print-length* 10)

;;; SPECIFIER-TYPE
;;;
;;; These tests ensure that SPECIFIER-TYPE signals errors for invalid
;;; type specifiers and coarsely check parsed types for valid type
;;; specifiers.

(with-test (:name (sb-kernel:specifier-type :forbidden-symbols))
  (mapc (lambda (spec)
          (twice ; repeat to test caches
            (assert (raises-error?
                     (sb-kernel:specifier-type spec)
                     sb-int:type-parse-error))))
        '(or not member eql satisfies values)))

(with-test (:name (sb-kernel:specifier-type :invalid-types))
  (macrolet ((test (specifier)
               `(twice ; repeat to test caches
                  (assert (raises-error?
                           (sb-kernel:specifier-type ',specifier)
                           sb-int:type-parse-error)))))

    ;; SPECIFIER-TYPE does accept VALUES types
    (test (values integer))
    (test (values &key)) ; key in VALUES specifier

    ;; Invalid FUNCTION types
    (test (function (&key :a))) ; malformed key
    (test (function (&key (:a integer) (:a integer)))) ; duplicate key
    (test (function (&aux))) ; &AUX not allowed
    (test (function (&aux integer)))

    ;; Invalid COMPLEX types
    (test (complex integer integer)) ; invalid syntax
    (test (complex symbol)) ; component type is not numeric
    (test (complex complex)) ; component type is not real

    ;; Invalid ARRAY types
    (test (array -1)) ; invalid syntax
    (test (array * :a)) ; invalid number of dimensions
    (test (array * -1)) ; negative number of dimensions
    (test (array * (:a))) ; invalid dimension
    (test (array * (-1))) ; negative dimension

    (test (simple-array -1))
    (test (simple-array * :a))
    (test (simple-array * -1))
    (test (simple-array * (:a)))
    (test (simple-array * (-1)))

    (test (vector -1))
    (test (vector * :a))
    (test (vector * -1))
    (test (vector * (:a)))
    (test (vector * (-1)))

    ;; Invalid NOT types
    (test (not integer float))

    ;; Invalid SIMD-PACK types
    #+sb-simd-pack (test (simd-pack integer integer)) ; invalid syntax
    #+sb-simd-pack (test (simd-pack ratio)) ; invalid element type
    #+sb-simd-pack (test (simd-pack symbol))))

(with-test (:name (sb-kernel:specifier-type :unknown-type))
  (twice ; repeat to test caches
    (assert
     (eq :ok
         (handler-case
             (assert (typep (sb-kernel:specifier-type 'no-such-type)
                            'sb-kernel:unknown-type))
           (sb-kernel:parse-unknown-type ()
             :ok))))))

;; Type specifiers of this form are specified as invalid in X3J13
;; Issue RECURSIVE-DEFTYPE.
(with-test (:name (sb-kernel:specifier-type :invalid-recursive-types
                                            :not-wellformed))
  (macrolet ((test (specifier)
               `(twice ; repeat to test caches
                 (assert (raises-error?
                          (sb-kernel:specifier-type ',specifier)
                          sb-int:type-parse-error)))))
    (test (and . #1=(t . #1#)))
    (test (or . #1#))
    (test (not . #1#))
    (test (member . #2=(:foo . #2#)))))

;; These type specifiers do not make sense.
(with-test (:name (sb-kernel:specifier-type :invalid-recursive-types
                                            :semantically-invalid))
  (macrolet ((test (specifier)
               `(twice ; repeat to test caches
                  (assert (raises-error?
                           (sb-kernel:specifier-type ',specifier)
                           sb-int:type-parse-error)))))
    (test #1=(and #1#))
    (test #2=(and t . #2#))
    (test #3=(or #3#))
    (test #4=(or t . #4#))
    (test #5=(not #5#))))

;; Valid recursive types

;; Produced /expansion/ is structurally recursive.
(deftype specifier-type.proper-list (&optional (element-type t))
  `(or null (cons ,element-type (proper-list ,element-type))))

;; The /expansion process/ is recursive.
(deftype specifier-type.list-of-length (length)
  (if (= 0 length)
      'null
      `(cons t (list-of-length ,(- length 1)))))

(with-test (:name (sb-kernel:specifier-type :valid-recursive-types))
  (macrolet ((test (specifier expected-ctype)
               `(twice ; repeat to test caches
                  (assert (typep (sb-kernel:specifier-type ',specifier)
                                 ',expected-ctype)))))
    (test #1=(and (cons t string) (cons #1#))  sb-kernel:cons-type)
    (test #2=(or null (cons integer #2#))      sb-kernel:union-type)
    (test #3=(not (cons symbol #3#))           sb-kernel:union-type) ; TODO correct?
    (test specifier-type.proper-list           sb-kernel:union-type)
    (test (specifier-type.proper-list t)       sb-kernel:union-type)
    (test (specifier-type.proper-list integer) sb-kernel:union-type)
    (test (specifier-type.list-of-length 0)    sb-kernel:member-type)
    (test (specifier-type.list-of-length 1)    sb-kernel:cons-type)
    (test (specifier-type.list-of-length 1024) sb-kernel:cons-type)))

;;; TYPE=

;; TODO test unknown types

(deftype type=.proper-list (&optional (element-type t))
  `(or null (cons ,element-type (type=.proper-list ,element-type))))

(deftype type=.list-of-length (length)
  (if (= 0 length)
      'null
      `(cons t (type=.list-of-length ,(- length 1)))))

(with-test (:name (type= :valid-recursive-types))
  (macrolet ((test (type1 type2 expected)
               `(twice ; repeat to test caches
                  (print ',type1) ;; TODO evaluated
                  (assert (eq ,expected
                              (sb-kernel:type=
                               (sb-kernel:specifier-type ',type1)
                               (sb-kernel:specifier-type ',type2)))))))
    ;; TODO add ≠ cases
    (test #1=(and (cons t string) (cons #1#)) #2=(and (cons t string) (cons #2#)) t)
    #+no (test #3=(or null (cons integer #3#))     #4=(or null (cons integer #4#))     t)
    #+no (test #5=(not (cons symbol #5#))          #6=(not (cons symbol #6#))          t)
    (test type=.proper-list                   type=.proper-list                   t)
    (test (type=.proper-list integer)         (type=.proper-list integer)         t)
    (test (type=.list-of-length 0)            (type=.list-of-length 0)            t)
    (test (type=.list-of-length 1)            (type=.list-of-length 1)            t)
    #+no (test (type=.list-of-length 1024)         (type=.list-of-length 1024)         t)))

#+no (defun g ()
  (sb-kernel:type=
   (sb-kernel:specifier-type '#1=(and (cons t string) (cons #1#)))
   (sb-kernel:specifier-type '#2=(and (cons t string) (cons #2#)))))

(sb-kernel::clear-type-caches)
(defun g ()
  (let ((sb-kernel::*break* t))
   (sb-kernel:type=
    (sb-kernel:specifier-type '#3=(or null (cons integer #3#)))
    (sb-kernel:specifier-type '#4=(or null (cons integer #4#))))))

#1=(cons integer (or null #1#))

;;; TYPEP

;; TODO test undefined types

(deftype typep.proper-list (&optional (element-type t))
  `(or null (cons ,element-type (typep.proper-list ,element-type))))

(deftype typep.list-of-length (length)
  (if (= 0 length)
      'null
      `(cons t (typep.list-of-length ,(- length 1)))))

(with-test (:name (typep :valid-recursive-types))
  (macrolet ((test (type object expected)
               `(twice ; repeat to test caches
                  (print ',type)
                  (assert (eq ,expected (typep ,object ',type)))
                  (assert (eq ,expected (eval '(typep ,object ',type)))))))
    (test #1=(and (cons t string) (cons #1#)) nil                         nil)
    (test #2=(and (cons t string) (cons #2#)) (cons 1 "foo")              nil)
    (test #3=(and (cons t string) (cons #3#)) (cons (cons 1 "foo") "bar") nil)
    (test #4=(and (cons t string) (cons #4#)) (let ((a (cons nil "bar")))
                                                (setf (car a) a))         t)
    ;; TODO more cases
    (test #5=(or null (cons integer #5#))     1                           nil)
    (test #6=(not (cons symbol #6#))          1                           nil)
    (test typep.proper-list                   1                           nil)
    (test (typep.proper-list integer)         1                           nil)
    (test (typep.list-of-length 0)            1                           nil)
    (test (typep.list-of-length 1)            1                           nil)
    ;; TODO too slow
    #+no (test (typep.list-of-length 1024)         1                           nil)))

#+no (sb-kernel::clear-type-caches)
(sb-kernel:specifier-type '(typep.proper-list t))
(sb-c::careful-specifier-type '(typep.proper-list t))

;; this works with patched %%TYPEP
(let ((a (cons nil "bar")))
  (setf (car a) a)
  (sb-kernel::%%typep
   a
   (sb-kernel:specifier-type '#1=(and (cons t string) (cons #1#)))))

(defun f ()
  (let ((a (cons nil "bar")))
    (setf (car a) a)
    (typep a '#1=(and (cons t string) (cons #1#)))))

(with-test (:name (typep :recursive-type :proper-list))
  (macrolet ((test (element-type object expected) ; TODO eval and compile
               `(assert (eq ,expected
                            (typep ,object
                                   `(typep.proper-list ,',element-type))))))
    ;;    element-type          object    expected
    (test t                     1          nil)
    (test t                     "foo"      nil)
    (test t                     '()        t)
    (test t                     '(:a)      t)
    (test t                     '(1)       t)

    (test integer               1          nil)
    (test integer               :foo       nil)
    (test integer               '(:foo)    nil)
    (test integer               '()        t)
    (test integer               '(1)       t)
    #+no (test integer               (make-list 100000 :initial-element 1) ; TODO too slow
                                           t)

    (test (or (eql :a) integer) 1          nil)
    (test (or (eql :a) integer) :foo       nil)
    (test (or (eql :a) integer) '(:foo)    nil)
    (test (or (eql :a) integer) '(:a :foo) nil)
    (test (or (eql :a) integer) '()        t)
    (test (or (eql :a) integer) '(:a)      t)
    (test (or (eql :a) integer) '(:a :a)   t)
    (test (or (eql :a) integer) '(1)       t)
    (test (or (eql :a) integer) '(1 1)     t)
    (test (or (eql :a) integer) '(:a 1)    t)
    (test (or (eql :a) integer) '(1 :a)    t)))

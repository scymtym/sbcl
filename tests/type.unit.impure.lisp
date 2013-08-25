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

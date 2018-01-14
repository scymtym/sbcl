(cl:defpackage "TEST-UTIL"
  (:use "CL" "SB-EXT")
  (:export
   #:with-test #:report-test-status #:*failures*
   #:really-invoke-debugger
   #:*break-on-failure* #:*break-on-expected-failure*

   ;; thread tools
   #:make-kill-thread #:make-join-thread

   ;; MAP-OPTIMIZATION-*
   #:map-optimization-quality-combinations
   #:map-optimize-declarations

   ;; CHECKED-COMPILE and friends
   #:checked-compile #:checked-compile-and-assert
   #:checked-compile-capturing-source-paths
   #:checked-compile-condition-source-paths

   #:runtime #:split-string #:shuffle

   ;; assertions
   #:grab-condition
   #:assert-error

   #:has-error?
   #:is
   #:assertoid
   #:assert-signal
   #:assert-no-signal

   #:legacy-eval-P
   #:equal-mod-gensyms
   #:check-function-evaluation-order))

(cl:defpackage "COMPILER-TEST-UTIL"
  (:nicknames "CTU")
  (:use "CL" "SB-C" "SB-KERNEL")
  (:export
   #:assert-consing
   #:assert-no-consing

   #:compiler-derived-type

   #:count-full-calls

   #:find-code-constants
   #:find-named-callees
   #:find-anonymous-callees

   #:file-compile))

(cl:defpackage "RUN-TESTS"
  (:use "CL" "TEST-UTIL" "SB-EXT")
  (:export
   #:run-all))

(cl:defpackage "SB-TEST"
  (:use "COMMON-LISP")
  (:documentation
   "TODO"))

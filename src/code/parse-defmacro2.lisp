;;;; the PARSE-DEFMACRO function and related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

#+no (in-package "SB!KERNEL")
(cl:defpackage #:defmacro-test2
  (:use #:cl)
  (:import-from #:sb-int
                #:ensure-list
                #:aver
                #:keywordicate
                #:with-unique-names
                #:neq
                #:once-only
                #:bad-type
                #:parse-body
                #:with-unique-names
                #:collect
                #:missing-arg
                #:named-lambda)
  (:import-from #:sb-kernel
                #:defmacro-bogus-sublist-error
                #:defmacro-error))
(in-package #:defmacro-test2)

;;; Lambda list sections

(defstruct (lambda-list-section (:copier nil))
  (name (missing-arg) :type symbol :read-only t)
  (keywords (missing-arg) :type list :read-only t)
  (guard (missing-arg) :type function :read-only t)
  (parser (missing-arg) :type function :read-only t)
  #+sb-doc (documentation nil :type (or null string) :read-only t))

;; TODO how does this relate to cl:lambda-list-keywords?
(declaim (type list #|of lambda-list-section|# *lambda-list-sections*))
(defvar *lambda-list-sections* '()
  #+sb-doc
  "Known lambda list sections.")

(defun lambda-list-keyword-p (symbol)
  (find symbol *lambda-list-sections*
        :test #'member
        :key #'lambda-list-section-keywords))

(defun find-lambda-list-section (name)
  (find name *lambda-list-sections*
        :key #'lambda-list-section-name))

(defun (setf find-lambda-list-section) (new-value name)
  (let ((old (find-lambda-list-section name)))
    (when old
      (setf *lambda-list-sections* (delete old *lambda-list-sections*))))
  (push new-value *lambda-list-sections*)
  new-value)

;;; Return a LAMBDA-LIST-SECTION instance within SECTIONS applicable
;;; to REST-OF-LAMBDA-LIST.
;;; POSITION and DEPTH specify TODO and TODO respectively.
(defun find-applicable-lambda-list-section (rest-of-lambda-list position depth
                                            &key (sections *lambda-list-sections*))
  (flet ((applicablep (lambda-list-section)
           (funcall (lambda-list-section-guard lambda-list-section)
                    rest-of-lambda-list position depth)))
    (find-if #'applicablep sections)))

;; used as default guard predicate for lambda list sections.
(defun lambda-list-section-applicable-when-keyword-matches (keyword)
  (lambda (rest-of-lambda-list position depth)
    (declare (ignore position depth))
    (eq keyword (car rest-of-lambda-list))))

(defmacro define-lambda-list-section (name-and-options &body body)
  #+sb-doc
  "TODO"
  (destructuring-bind (name &key (keyword name) aliases guard
                                 valid-position valid-depth before)
      (ensure-list name-and-options)
    (declare (type symbol name)
             (type (or symbol null) keyword)
             (type list aliases))
    (multiple-value-bind (body declarations documentation) (parse-body body)
      (declare (ignore declarations #-sb-doc documentation))
      (with-unique-names (section)
        `(let ((,section (make-lambda-list-section
                          :name ',name
                          :keywords '(,@(when keyword `(,keyword)) ,@aliases)
                          :guard ,(if guard
                                      `(lambda (rest position depth)
                                         (declare (ignore position depth))
                                         (let ((head (car rest)))
                                           ,guard))
                                      `(lambda-list-section-applicable-when-keyword-matches ',keyword))
                          :parser (lambda (rest position depth
                                           variable seen
                                           rest-var)
                                    (declare (ignorable position depth))
                                    (let ((head (first rest))
                                          (consumed 0))
                                      (labels ((seen (&rest args) (apply seen args))
                                               (variable (&rest args) (apply variable args))
                                               (try-consume (&key type allow-keyword)
                                                 (when (and head
                                                            (or allow-keyword
                                                                (not (lambda-list-keyword-p head)))
                                                            (or (not type) (typep head type)))
                                                   (incf consumed)
                                                   (multiple-value-prog1
                                                       (values head t)
                                                     (pop rest)
                                                     (setf head (first rest)))))
                                               (consume (&rest args)
                                                 (multiple-value-bind (thing found?)
                                                     (apply #'try-consume args)
                                                  (if found?
                                                      (values thing found?)
                                                      (defmacro-error (format nil "~A ~S" ',keyword head) 'TODO-context 'TODO-name)))))
                                        ,@(when valid-depth
                                            `((unless (typep depth ',valid-depth)
                                                (error "~A parameter section may only appear at depth ~A of lambda list (not ~D)"
                                                       ',name ',valid-depth depth))))
                                        ,@(when valid-position
                                            `((unless (typep position ',valid-position)
                                                (error "~A parameter section may only appear at position ~A of lambda list (not ~D)"
                                                       ',name ',valid-position position))))
                                        ,@(when before
                                            `((when (some #'seen ',before)
                                                (error "~A parameter section after ~A in ~A"
                                                       ',name (find-if #'seen ',before) 'TODO-context))))
                                        ,@(when keyword
                                            `((consume :allow-keyword t)))
                                        (seen ',name t)
                                        (multiple-value-bind (variables minimum maximum new-rest-var)
                                            (progn ,@body)
                                          (values variables consumed minimum maximum new-rest-var)))))
                          #+sb-doc :documentation #+sb-doc ,documentation)))
           (setf (find-lambda-list-section ',name) ,section))))))

(defun map-parameters (function stepper)
  (do ((parameter (multiple-value-list (funcall stepper))
                  (multiple-value-list (funcall stepper))))
      ((null (second parameter)))
    (funcall function (first parameter))))

(defmacro do-parameters ((parameter-var stepper) &body body)
  `(map-parameters (lambda (,parameter-var) ,@body) ,stepper))

(defun normalize-singleton-variable (variable &optional (default #+TODO *default-default*))
  (when (null (cdr variable))
    (setf (cdr variable) (list default)))
  variable)

(defun process-variable-or-nested-defmacro-lambda-list
    (var-or-pattern depth rest-var name context
     variable &optional (binding (lambda (whole)
                                   (let ((rest-var (gensym "REST")))
                                     (values `(car ,whole) `(,rest-var (cdr ,whole)))))))
  (if (listp var-or-pattern)
      (multiple-value-bind (entries minimum maximum rest-var) ; TODO unused
          (parse-defmacro-lambda-list var-or-pattern (1+ depth) rest-var name context
                                      :note-variable variable)
        (list :nested entries minimum maximum binding))
      (funcall variable var-or-pattern binding)))

;;; Builtin lambda list sections

(progn ; TODO only for development

  (define-lambda-list-section (&whole :valid-position (eql 0))
    "&WHOLE var-or-pattern"
    (let ((var-or-pattern (consume)))
      (values
       (list
        (process-variable-or-nested-defmacro-lambda-list
         var-or-pattern depth rest-var 'TODO-name 'TODO-context #'variable #'identity))
       0 0 rest-var)))

  (define-lambda-list-section (&environment :valid-depth (eql 0))
    "&ENVIRONMENT var
   Can appear at any position in a top-level lambda list but not in
   nested lambda lists."
    (let ((name (consume :type '(and symbol (not (or null keyword)))))) ; TODO legal-variable-name-p
      (values (variable name) 0 0 rest-var)))

  (define-lambda-list-section (:required
                               :keyword nil
                               :before (&optional &rest &key &aux)
                               :guard (not (lambda-list-keyword-p head))) ; TODO consider only "active" lambda-list keywords
    "var-or-pattern*"
    (collect ((parameters))
      (do-parameters (var-or-pattern #'try-consume)
        (parameters (process-variable-or-nested-defmacro-lambda-list
                     var-or-pattern depth rest-var 'TODO-name 'TODO-context #'variable)))
      (let ((count (length (parameters))))
        (values (parameters) count count rest-var))))

  (defun parse-optional-variable (var-or-pattern)
    var-or-pattern)

  (define-lambda-list-section (&optional :before (&rest &key &aux))
    "&OPTIONAL {var-or-pattern | (var-or-pattern [init-form [supplied-p-parameter]])}*"
    (collect ((parameters))
      (do-parameters (var-or-pattern #'try-consume)
        (setf rest-var (gensym "REST"))
        (multiple-value-bind (name-or-pattern default supplied-p-variable)
            (parse-optional-variable var-or-pattern)
          (parameters
           (process-variable-or-nested-defmacro-lambda-list
            name-or-pattern depth rest-var 'TODO-name 'TODO-context #'variable)
           #+no (list

            'default
            'suppliedp-name))))
      (let ((count (length (parameters))))
        (values (parameters) 0 count rest-var))))

  (define-lambda-list-section (&rest :aliases (&body) :before (&key &aux))
    "{&REST | &BODY} var-or-pattern"
    (let ((var-or-pattern (consume)))
      (values (list (variable var-or-pattern)) 0 nil rest-var)))

  (defun parse-keyword-parameter (var-or-pattern)
    (destructuring-bind (keyword-and-name &optional default suppliedp-name)
        (ensure-list var-or-pattern)
      (let ((keyword-supplied? (consp keyword-and-name)))
        (destructuring-bind (keyword &optional name-or-pattern)
            (if keyword-supplied?
                keyword-and-name
                (list (keywordicate keyword-and-name) keyword-and-name))
          (check-type keyword keyword)
          (check-type name-or-pattern (or cons (and symbol (not (or keyword null)))))
          (values name-or-pattern keyword default suppliedp-name)))))

  (define-lambda-list-section (&key :before (&aux))
    "&key {var-or-pattern | ({var-or-pattern | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]"
    (collect ((parameters))
      (do-parameters (var-or-pattern (lambda ()
                                       (try-consume :type '(not (eql &allow-other-keys)))))
        (multiple-value-bind (name-or-pattern keyword default suppliedp-name)
            (parse-keyword-parameter var-or-pattern)
          (parameters
           (process-variable-or-nested-defmacro-lambda-list
            name-or-pattern depth rest-var 'TODO-name 'TODO-context #'variable)
           #+no (list

            keyword default suppliedp-name))))
      (let* ((a-o-k (try-consume :type '(eql &allow-other-keys)))
             (count (length (parameters)))
             (maximum (unless (or (plusp count) a-o-k) 0)))
        (values (list* a-o-k (parameters)) 0 maximum rest-var))))

  (define-lambda-list-section &aux
    "&aux {var | (var [init-form])}*"
    (let ((name (consume)))
      (values (variable name) 0 0 rest-var))))

;;; Parsing machinery

;; ANSI specifies that dotted lists are "treated exactly as if the
;; parameter name that ends the list had appeared preceded by &REST."
;; We force this behavior by transforming dotted lists into ordinary
;; lists with explicit &REST elements.
(defun normalize-possibly-dotted-lambda-list (possibly-dotted-lambda-list context)
  (unless (listp possibly-dotted-lambda-list)
    (bad-type possibly-dotted-lambda-list 'list "~S lambda list is not a list: ~S"
              context possibly-dotted-lambda-list))
  (do ((in-pdll possibly-dotted-lambda-list (cdr in-pdll))
       (reversed-result ()))
      ((atom in-pdll)
       (nreverse (if in-pdll
                     (list* in-pdll '&rest reversed-result)
                     reversed-result)))
    (push (car in-pdll) reversed-result)))

(defun make-variable-table (context)
  (let ((variables ()))
    (named-lambda note-variable (&optional name init-form) ; TODO not used
      (when (member name variables :key #'car)
        (error "~@<Variable ~S occurs more than once in ~A lambda list.~@:>"
               name context))
      (let ((cell (cons name init-form)))
       (push cell variables)
        cell))))

(defun make-section-table (context)
  (let ((seen (make-hash-table)))
    (named-lambda seen (which &optional value (if-exists :error))
      (cond
        ((not value)
         (gethash which seen))
        ((and (eq if-exists :error) (gethash which seen))
         (error "~@<Multiple ~A sections in ~A lambda list.~@:>"
                which context))
        ((or (eq if-exists :replace) (not (gethash which seen)))
         (setf (gethash which seen) value))))))

;;; Parse POSSIBLY-DOTTED-LAMBDA-LIST and return three values
;;; 1) A plist of recognized lambda list sections of the form
;;;
;;;      (NAME1 ENTRIES1 NAME2 ENTRIES2 ...)
;;;
;;;    where NAMEN are names such as &whole or &key and ENTRIESN are
;;;    lists of entries in the respective sections. Each of these
;;;    entries is of the form:
;;;
;;;      (:nested . ENTRIES MINIMUM MAXIMUM BINDING)
;;;      (NAME . INITIAL-VALUE)
;;;
;;; 2) Minimum number of arguments required by
;;;    POSSIBLY-DOTTED-LAMBDA-LIST
;;; 3) Maximum number of arguments required by
;;;    POSSIBLY-DOTTED-LAMBDA-LIST
(defun parse-defmacro-lambda-list (possibly-dotted-lambda-list depth
                                   whole-var name context
                                   &key
                                   error-fun
                                   anonymousp
                                   env-illegal
                                   sublist
                                   (allowed-sections *lambda-list-sections*)
                                   (note-variable (make-variable-table context)))
  (let* ((lambda-list (normalize-possibly-dotted-lambda-list
                       possibly-dotted-lambda-list context))
         (rest lambda-list)
         (rest-var whole-var)
         (note-section (make-section-table context))
         (minimum 0)
         (maximum 0))
    (collect ((stuff))
      (do ((position 0)) ((null rest))
        (let ((section (find-applicable-lambda-list-section
                        rest position depth :sections allowed-sections)))
          (multiple-value-bind (entries consumed
                                section-minimum section-maximum
                                new-rest-var)
              (funcall (lambda-list-section-parser section)
                       rest position depth note-variable note-section rest-var)
            ;; Advance in input.
            (incf position consumed)
            (setf rest (nthcdr consumed rest)
                  rest-var new-rest-var)
            ;; Collect production.
            (stuff (lambda-list-section-name section) entries)
            ;; Adjust minimum and maximum acceptable number of
            ;; arguments.
            (incf minimum section-minimum)
            (setf maximum (when (and maximum section-maximum)
                            (+ maximum section-maximum))))))
      (values (stuff) minimum maximum))))

(parse-defmacro-lambda-list '(&whole boo) 0 'whole nil 'destructuring-bind2)

;;; Return binding forms suitable for LET* for the elements of
;;; LAMBDA-LIST-ENTRIES.
;;; The elements of LAMBDA-LIST-ENTRIES have to be of the form:
;;;
;;;   (
(defun make-defmacro-bindings (lambda-list-entries whole-var minimum maximum)
  (collect ((bindings))
    (let* ((checked-var (gensym "CHECKED"))
           (rest-var checked-var))
      (bindings `(,checked-var
                  (,(if (not maximum)
                        'sb-impl::check-list-of-length-at-least
                        'sb-impl::check-proper-list-of-length)
                   ,whole-var ,minimum ,@(when maximum `(,maximum))
                   ,@(when nil #+no name `(:name 'TODO-name))
                   :kind 'TODO-context
                   :lambda-list 'TODO-lambda-list
                   :signal-via 'TODO-error-fun)))
      (labels ((process (spec)
                 (destructuring-bind (name . value) spec
                   (case name
                     (:nested
                      (multiple-value-bind (value-form next-rest-var) (funcall (fourth value) rest-var)
                        (mapc (lambda (x) (bindings x))
                              (make-defmacro-bindings (first value) value-form  (second value) (third value))) ; TODO pass collector into recursive call
                        (when next-rest-var
                          (bindings next-rest-var)
                          (setf rest-var (first next-rest-var)))))
                     (t
                      (multiple-value-bind (value-form next-rest-var) (funcall value rest-var)
                        (bindings (list name value-form))
                        (when next-rest-var
                          (bindings next-rest-var)
                          (setf rest-var (first next-rest-var))))))))
               (foo (spec)
                 (loop for (name entries) on spec :by #'cddr
                       do (case name
                            (&key (mapc #'process (rest entries)))
                            (t (mapc #'process entries))))))
        (foo lambda-list-entries))
      (bindings))))

;;; Parse LAMBDA-LIST and BODY and return, as multiple values:
;;; 1) a body (without documentation string or declarations)
;;; 2) possibly a DECLARE form to put where this code is inserted
;;; 3) the documentation for the parsed body
;;; 4) minimum number of arguments
;;; 5) maximum number of arguments
(defun parse-defmacro2 (lambda-list whole-var body name context
                        &key
                        (anonymousp nil)
                        (doc-string-allowed t)
                        ((:environment env-arg-name))
                        ((:default-default default-default #+no *default-default*))
                        (error-fun 'error)
                        (wrap-block t)
                        (allowed-sections ; TODO whitelist allowed sections here?
                         (remove '&more *lambda-list-sections*
                                 :key #'lambda-list-section-name)))
  (multiple-value-bind (lambda-list-entries minimum maximum)
      (parse-defmacro-lambda-list lambda-list 0 whole-var name context
                                  :allowed-sections allowed-sections)
    (multiple-value-bind (body declarations documentation)
        (parse-body body :doc-string-allowed doc-string-allowed)
      (values `(let* ,(make-defmacro-bindings
                       lambda-list-entries whole-var minimum maximum)
                 #-sb-xc-host
                 #+TODO (declare (muffle-conditions sb-ext:code-deletion-note))
                 ,@declarations
                 ,@body)
              '()
              documentation minimum maximum))))

;;; Tests and experiments

(defmacro destructuring-bind2 (lambda-list expression &body body)
  (let ((whole-name (gensym "WHOLE")))
    (multiple-value-bind (body local-declarations)
        (parse-defmacro2 lambda-list whole-name body nil 'destructuring-bind
                         :anonymousp t
                         :doc-string-allowed nil
                         :wrap-block nil)
      `(let ((,whole-name ,expression))
         ;; This declaration-as-assertion should protect us from
         ;; (DESTRUCTURING-BIND (X . Y) 'NOT-A-LIST ...).
         (declare (type list ,whole-name))
         ,@local-declarations
         ,body))))

(destructuring-bind2 (a b c d e) '(1 2 3 4 5)
  (values a b c d e))

(destructuring-bind2 (&whole boo blah (bla3 &optional foo bar &key bla4)
                             bli &key foo2 &allow-other-keys)
    '(1 (2 3 4 5 :bla4 6) 7 :foo 8)
  (list boo blah bla3 foo bar bla4 bli foo2))

#+no (destructuring-bind2 (&whole (foo bar) &rest r &aux ((fez a) 1)) '(1 2)
  (list foo bar fez))

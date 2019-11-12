;;;; External formats

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Utilities

(deftype error-policy ()
  '(or null function-name function))

 ;;; EXTERNAL-FORMAT

(defstruct (external-format
             (:constructor %make-external-format)
             (:conc-name ef-)
             (:predicate external-format-p)
             (:copier %copy-external-format))
  ;; All the names that can refer to this external format.  The first
  ;; one is the canonical name.
  (names (missing-arg) :type list :read-only t)
  (default-replacement-character (missing-arg) :type character)
  (read-n-chars-fun (missing-arg) :type function)
  (read-char-fun (missing-arg) :type function)
  (write-n-bytes-fun (missing-arg) :type function)
  (write-char-none-buffered-fun (missing-arg) :type function)
  (write-char-line-buffered-fun (missing-arg) :type function)
  (write-char-full-buffered-fun (missing-arg) :type function)
  ;; Can be nil for fixed-width formats.
  (resync-fun nil :type (or function null))
  (bytes-for-char-fun (missing-arg) :type function)
  (read-c-string-fun (missing-arg) :type function)
  (write-c-string-fun (missing-arg) :type function)
  ;; We indirect through symbols in these functions so that a
  ;; developer working on the octets code can easily redefine things
  ;; and use the new function definition without redefining the
  ;; external format as well.  The slots above don't do any
  ;; indirection because a developer working with those slots would be
  ;; redefining the external format anyway.
  (octets-to-string-fun (missing-arg) :type function)
  (string-to-octets-fun (missing-arg) :type function))
(declaim (freeze-type external-format))

(defun ef-char-size (ef-entry)
  (if (variable-width-external-format-p ef-entry)
      (bytes-for-char-fun ef-entry)
      (funcall (bytes-for-char-fun ef-entry) #\x)))

(defun sb-alien::string-to-c-string (string external-format)
  (declare (type simple-string string)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (find-external-format external-format)))
      (funcall (ef-write-c-string-fun external-format) string))))

(defun sb-alien::c-string-to-string (sap external-format element-type)
  (declare (type system-area-pointer sap)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (find-external-format external-format)))
      (funcall (ef-read-c-string-fun external-format) sap element-type))))

(defun wrap-external-format-functions (external-format function)
  (let ((result (%copy-external-format external-format)))
    (macrolet ((frob (accessor)
                 `(setf (,accessor result) (funcall function (,accessor result)))))
      (frob ef-read-n-chars-fun)
      (frob ef-read-char-fun)
      (frob ef-write-n-bytes-fun)
      (frob ef-write-char-none-buffered-fun)
      (frob ef-write-char-line-buffered-fun)
      (frob ef-write-char-full-buffered-fun)
      (frob ef-resync-fun)
      (frob ef-bytes-for-char-fun)
      (frob ef-read-c-string-fun)
      (frob ef-write-c-string-fun)
      (frob ef-octets-to-string-fun)
      (frob ef-string-to-octets-fun))
    result))

(defun external-format-keyword (spec) ; TODO unused?
  (typecase spec
    (keyword spec)
    ((cons keyword) (car spec))))

(defun canonize-external-format (spec external-format)
  (let ((name (first (ef-names external-format))))
    (typecase spec
      (keyword
       name)
      ((cons keyword)
       (cons name (rest spec))))))

(defun external-format-designator-to-key (designator
                                          &key
                                          default-replacement)
  (destructuring-bind (character-coding-name
                       &key
                       (replacement default-replacement))
      (ensure-list designator)
    (list character-coding-name replacement)))

(define-load-time-global *external-formats* (make-hash-table)
  "Hashtable of all available external formats. The table maps from
external-format names to EXTERNAL-FORMAT structures.")

;;; Keys are lists of the form
;;;
;;;   (CHARACTER-CODING-NAME REPLACEMENT)
;;;
;;; .
(declaim (type hash-table **external-format-cache**))
(define-load-time-global **external-format-cache**
    (make-hash-table :test #'equal))

(defun replacement-handlerify (entry replacement)
  (when entry
    (wrap-external-format-functions
     entry (lambda (fun)
             (and fun (sb-kernel::replacement-handlerify-function
                       fun replacement))))))

(defun external-format-for-spec (spec)
  (destructuring-bind (character-coding-name replacement) spec
    (let ((character-coding (gethash character-coding-name *external-formats*)))
      (when character-coding
        (values (lambda ()
                  (replacement-handlerify character-coding replacement))
                (list (first (ef-names character-coding)) replacement))))))

;;; Try to find the external format designated by SPEC (which is the
;;; result of calling EXTERNAL-FORMAT-DESIGNATOR-TO-KEY on a
;;; designator) in **EXTERNAL-FORMAT-CACHE**.
;;;
;;; If there is no matching entry
;;; 1. Make a suitable EXTERNAL-FORMAT instance or an error condition
;;;    by calling EXTERNAL-FORMAT-FOR-SPEC
;;; 2. Make a hash-table that is like the one stored in
;;;    **EXTERNAL-FORMAT-CACHE** but additionally contains the result
;;;    of 1.
;;; 3. Atomically install that table as the value of
;;;    **EXTERNAL-FORMAT-CACHE**
;;;
;;; Return the EXTERNAL-FORMAT instance or NIL.
(defun %find-external-format (spec)
  (or (gethash spec **external-format-cache**)
      (binding* (((external-format canonical-spec)
                  (external-format-for-spec spec))
                 (external-format
                  (cond ((gethash canonical-spec **external-format-cache**))
                        ((functionp external-format)
                         (funcall external-format))
                        (t
                         external-format))))
        (when external-format
          (flet ((new-table (old-table)
                   (let* ((test (hash-table-test old-table))
                          (new-table (make-hash-table :test test)))
                     (maphash (lambda (key value)
                                (setf (gethash key new-table) value))
                              old-table)
                     (setf (gethash spec new-table) external-format)
                     new-table)))
            (loop for old-table = **external-format-cache**
                  for new-table = (new-table old-table)
                  until (eq old-table (cas **external-format-cache**
                                           old-table new-table)))))
        external-format)))

(declaim (ftype (sfunction (external-format-designator
                            &key (:if-does-not-exist error-policy))
                           (or null external-format))
                find-external-format))
(defun find-external-format (designator &key (if-does-not-exist #'error))
  "Return the external format designated by DESIGNATOR which can be of
one of the following forms

  KEYWORD

    Name of a character coding.

  (KEYWORD &key REPLACEMENT)

    Character coding named by KEYWORD

IF-DOES-NOT-EXIST controls the behavior in case the requested
character-coding cannot be found:

A function

  If DESIGNATOR does not designate an external format, call
  IF-DOES-NOT-EXIST with an error condition as the sole argument.

Any other value

  Return IF-DOES-NOT-EXIST if DESIGNATOR does not designate an
  external format.

EXPERIMENTAL: Interface subject to change."
  (when (eq designator :default)
    (return-from find-external-format
      (find-external-format (default-external-format)
                            :if-does-not-exist if-does-not-exist)))

  (let* ((key (external-format-designator-to-key designator))
         (result (%find-external-format key)))
    (cond ((external-format-p result)
           result)
          ((functionp if-does-not-exist)
           (apply if-does-not-exist result))
          (t
           if-does-not-exist))))


;;; Default external format

(defvar *default-external-format* nil)

#-win32
(defun unix-default-codeset ()
  (let ((code-set (or #-android
                      (alien-funcall
                       (extern-alien
                        "nl_langinfo"
                        (function (c-string :external-format :latin-1) int))
                       sb-unix:codeset)
                      "LATIN-1")))
    (find-symbol code-set *keyword-package*)))

(defun default-external-format ()
  (or *default-external-format*
      ;; On non-unicode, use iso-8859-1 instead of detecting it from
      ;; the locale settings. Defaulting to an external-format which
      ;; can represent characters that the CHARACTER type can't
      ;; doesn't seem very sensible.
      #-sb-unicode
      (setf *default-external-format* :latin-1)
      #+sb-unicode
      (let ((external-format #-win32 (unix-default-codeset)
                             #+win32 (sb-win32::ansi-codepage)))
        (let ((entry (find-external-format
                      external-format :if-does-not-exist nil)))
          (cond (entry
                 (/show0 "matched"))
                (t
                 ;; FIXME! This WARN would try to do printing before
                 ;; the streams have been initialized, causing an
                 ;; infinite erroring loop. We should either print it
                 ;; by calling to C, or delay the warning until
                 ;; later. Since we're in freeze right now, and the
                 ;; warning isn't really essential, I'm doing what's
                 ;; least likely to cause damage, and commenting it
                 ;; out. This should be revisited after 0.9.17.
                 ;;   -- JES, 2006-09-21
                 #+nil
                 (warn "Invalid external-format ~A; using LATIN-1"
                       external-format)
                 (setf external-format :latin-1))))
        (setf *default-external-format* external-format))))


;;;; Backward compatibility

(defun get-external-format (spec)
  (find-external-format spec :if-does-not-exist nil))

(defun get-external-format-or-lose (spec)
  "Like GET-EXTERNAL-FORMAT, but signal an error instead of returning NIL."
  (find-external-format spec :if-does-not-exist #'error))

(declaim (deprecated
          :early ("SBCL" "1.4.12")
          (function get-external-format :replacement find-external-format)
          (function get-external-format-or-lose :replacement find-external-format)))

#.(progn
    (ql:quickload '(:alexandria :cl-dot))
    '(ql:quickload '(:alexandria :cl-dot)))

;; TODO: look at http://metamodular.com/Rewrite/rewrite.html for inspiration

#|
digraph G {
compound=true;
subgraph cluster_foo {
1 [label="1 in foo"];

}
subgraph cluster_bar {
2 [label="2 in bar"];
}
1 -> 2 [ltail=cluster_foo,
lhead=cluster_bar];
}
|#

(cl:defpackage #:sb-ir1-visualization
  (:use
   #:cl
   #:cl-dot)

  (:export
   #:compile-with-phase-graphs))

(cl:in-package #:sb-ir1-visualization)

;;; Utility functions

(defun list-no-nil (&rest args)
  (remove nil args))

(defun attributify-edge (object &rest attribute-plist)
  (make-instance 'attributed :object object
                 :attributes attribute-plist))

(defun boldify-edge (object &rest attribute-plist)
  (apply #'attributify-edge object
         :style :bold :weight 10000
         attribute-plist))

(defun color-edge (object color &rest attribute-plist)
  (apply #'attributify-edge object
         :color color
         attribute-plist))

;;; Graph class

(defparameter *showable-things*
  '(:ctran :lvar
    (:block :head) (:block :tail) (:block :last)
    :lambda :lambda-var))

(defun showable-things-p (things)
  (and (listp things)
       (null (set-difference things *showable-things* :test #'equal))))

(deftype showable-things ()
  `(satisfies showable-things-p))

(defclass ir1-graph ()
  ((show :initarg  :show
         :type     showable-things
         :reader   ir1-graph-show
         :initform '(:lambda :lambda-var))))

(defmethod showp ((graph ir1-graph) (thing t))
  (member thing (ir1-graph-show graph) :test #'equal))

;;;

(defmethod graph-object-node ((graph ir1-graph) (object null))
  nil)

;;; COMPONENT

(defmethod graph-object-node ((graph ir1-graph) (object sb-c:component))
  (let ((label (format nil "Component ~A" (sb-c::component-name object))))
    (make-instance 'cl-dot::cluster :attributes `(:label ,label))))

(defmethod graph-object-contains #+no knows-of ((graph ir1-graph) (c sb-c:component))
  (list* (sb-c::component-head c)
         (when (showp graph :lambda)
           (sb-c::component-lambdas c))))

;;; CBLOCK

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::cblock))
  (let* ((component (sb-c::block-component c))
         (special   (cond
                      ((eq c (sb-c::component-head component))
                       :head)
                      ((eq c (sb-c::component-tail component))
                       :tail)
                      ((eq c (sb-c::component-last-block component))
                       :last))))
    (when (or (not special) (showp graph `(:block ,special)))
      (let ((label (format nil "Block ~A~@[ [~A]~]"
                           (sb-c::block-number c) special)))
        (make-instance 'cl-dot::cluster :attributes `(:label ,label))))))

#+no (defmethod graph-object-points-to ((graph ir1-graph) (c sb-c::cblock))
       (mapcar #'boldify-edge (sb-c::block-succ c)))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::cblock))
  (append (sb-c::block-pred c)
          (sb-c::block-succ c)
          (list-no-nil (sb-c::block-component c) (sb-c::block-start c))))

(defmethod graph-object-contained-by ((graph ir1-graph) (c sb-c::cblock))
  (or (when (showp graph :lambda)
        (sb-c::block-home-lambda-or-null c))
      (sb-c::block-component c)))

;;; CLAMBDA

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::clambda))
  (let ((label (format nil "Lambda ~A" (sb-c::lambda-%source-name c))))
    (make-instance 'cl-dot::cluster :attributes `(:label ,label))))

(defmethod graph-object-contains ((graph ir1-graph) (c sb-c::clambda))
  (when (showp graph :lambda-var)
    (sb-c::lambda-vars c)))

;;; LAMBDA-VAR

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::lambda-var))
  (let ((label (format nil "Var ~A" (sb-c::lambda-var-%source-name c))))
    (make-instance 'node :attributes `(:label ,label))))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::lambda-var))
  #+no (sb-c::lambda-var-refs c))

;;; CTRAN

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::ctran))
  (when (showp graph :ctran)
    (let ((label (format nil "CTRAN ~D ~A"
                         (sb-c::cont-num c)
                         (sb-c::ctran-kind c))))
      (make-instance 'cl-dot::cluster :attributes `(:label ,label)))))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::ctran))
  (list-no-nil (sb-c::ctran-next c)))

(defmethod graph-object-points-to ((graph ir1-graph) (c sb-c::ctran))
  (list-no-nil #+no (sb-c::ctran-use c)))

(defmethod graph-object-contains ((graph ir1-graph) (c sb-c::ctran))
  (list-no-nil (sb-c::ctran-next c)))

(defmethod graph-object-contained-by ((graph ir1-graph) (c sb-c::ctran))
  (sb-c::ctran-block c))

;; NODE

(defmethod graph-object-points-to ((graph ir1-graph) (c sb-c::node))
  (append (list-no-nil (sb-int:awhen (sb-c::node-next c)
                         (boldify-edge (if (showp graph :ctran)
                                           sb-int:it
                                           (sb-c::ctran-next sb-int:it)))))
          (unless (sb-c::node-next c)
            (sb-int:awhen (sb-c::block-succ (sb-c::ctran-block (sb-c::node-prev c)))
              (mapcar #'boldify-edge sb-int:it)))))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::node))
  (list-no-nil (sb-c::node-prev c)))

;;; REF

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::ref))
  (let ((label (if (or (showp graph :lvar) (not (sb-c::ref-lvar c)))
                   "REF"
                   (multiple-value-bind (kind leaf type value)
                       (lvar-info (sb-c::ref-lvar c))
                     (format nil "REF ~A\\n~@[~A~]~@[ : ~A~]~@[\\n = ~S~]"
                             kind leaf type value)))))
   (make-instance 'node :attributes `(:label     ,label
                                      :fillcolor "#ddffbb"
                                      :style     :filled
                                      :shape     :ellipse))))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::ref))
  (append (call-next-method) (list-no-nil (sb-c::ref-lvar c))))

;;; BIND

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::bind))
  (make-instance 'node :attributes `(:label ,"BIND")))

;;; GLOBAL-VAR

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::global-var))
  (let ((label (format nil "GLOBAL-VAR\\n~A\\n~A"
                       (sb-c::global-var-%source-name c)
                       (sb-c::global-var-kind c))))
    (make-instance 'node :attributes `(:label ,label :shape :box))))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::global-var))
  (sb-c::global-var-refs c))

;;; CONSTANT

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::constant))
  (let ((label (format nil "CONSTANT ~A"
                       #+nil (sb-c::lvar- source c)
                       (sb-c::constant-value c))))
    (make-instance 'node :attributes `(:label     ,label
                                       :style     :filled
                                       :fillcolor "#ffffe0"
                                       :shape     :box))))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::constant))
  (sb-c::constant-refs c))

;;; COMBINATION

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::basic-combination))
  (let ((label (format nil "~(~@[~A ~]~A ~A~)\\n~A\\n: ~A"
                       (when (sb-c::node-tail-p c) "tail")
                       (sb-c::basic-combination-kind c)
                       (type-of c)
                       (sb-c::lvar-fun-debug-name ; sb-c::lvar-fun-name
                        (sb-c::basic-combination-fun c))
                       (sb-c::type-specifier
                        (sb-c::basic-combination-derived-type c)))))
    (make-instance 'node :attributes `(:label     ,label
                                       :shape     :octagon
                                       :style     :filled
                                       :fillcolor "#ccffff"))))

(defmethod graph-object-points-to ((graph ir1-graph) (c sb-c::basic-combination))
  (append (call-next-method)
          (apply #'list-no-nil
                 (sb-int:awhen (sb-c::basic-combination-lvar c)
                   (attributify-edge sb-int:it :style :bold :color "#9999ff"))
                 (sb-int:awhen (sb-c::basic-combination-fun c)
                   (attributify-edge sb-int:it :style :bold :color "#0000ff"))
                 #+no (sb-c::basic-combination-args c)
                 '())))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::basic-combination))
  (append (call-next-method) (sb-c::basic-combination-args c)))

;;; CIF

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::cif))
  (make-instance 'node :attributes '(:label "CIF"
                                     :shape :diamond)))

(defmethod graph-object-points-to ((graph ir1-graph) (c sb-c::cif))
  (list-no-nil #+no (sb-c::if-test c)
               (boldify-edge (sb-c::if-consequent c) :color "green")
               (boldify-edge (sb-c::if-alternative c) :color "red")))

(defmethod graph-object-pointed-to-by ((graph ir1-graph) (c sb-c::cif))
  (list-no-nil (sb-c::if-test c)))

;;; ENTRY

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::entry))
  (make-instance 'node :attributes '(:label "" ; "ENTRY"
                                     :shape :circle)))

;;; EXIT

(defmethod graph-object-node ((graph ir1-graph) (c sb-ext:exit))
  (make-instance 'node :attributes '(:label "" ; "EXIT"
                                     :shape :doublecircle)))

#+no (defmethod graph-object-knows-of ((graph ir1-graph) (c sb-ext:exit))
  (list-no-nil (sb-c::exit- c)
               (sb-c::return-result c)))

;;; CRETURN

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::creturn))
  (make-instance 'node :attributes '(:label "CRETURN")))

(defmethod graph-object-knows-of ((graph ir1-graph) (c sb-c::creturn))
  (append (call-next-method) (list-no-nil (sb-c::return-result c))))

;;; CAST

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::cast))
  (let ((label (format nil "CAST\\n~A" (sb-c::cast-asserted-type c))))
    (make-instance 'node :attributes `(:label     ,label
                                       :style     :filled
                                       :fillcolor "#ffccff"))))

;;; CSET

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::cset))
  (multiple-value-bind (kind leaf type value) (lvar-info (sb-c::set-value c))
    (declare (ignore kind leaf))
    (let ((label (format nil "CSET ~A <- ~A\\n: ~A"
                         (sb-c::basic-var-%source-name (sb-c::set-var c))
                         value type)))
      (make-instance 'node :attributes `(:label     ,label
                                         :shape     :rectangle
                                         :style     :filled
                                         :fillcolor "#ffcccc")))))

(defmethod graph-object-points-to ((graph ir1-graph) (c sb-c::cset))
  (append (call-next-method) (list-no-nil (sb-c::set-value c))))

;;; LVAR

(defun lvar-info (lvar)
  (let* (#+no (combination-result-p (let ((use (sb-c::principal-lvar-use c)))
                                      (typep use 'sb-c::basic-combination)))
         (combination-fun-p (let ((dest (sb-c::lvar-dest lvar)))
                              (and (typep dest 'sb-c::basic-combination)
                                   (eq (sb-c::basic-combination-fun dest) lvar))))
         (kind (cond
                 (combination-fun-p "FUN LVAR")
                 (t                 "LVAR")))
         (leaf (let ((ref (sb-c::lvar-uses lvar)))
                 (when (sb-c::ref-p ref)
                   (let ((leaf (sb-c::ref-leaf ref)))
                     (when (sb-c::leaf-has-source-name-p leaf)
                       (sb-c::leaf-source-name leaf))))))
         (type (let ((type (sb-c::type-specifier (sb-c::lvar-type lvar))))
                 ;; Too much noise for function types.
                 (if (subtypep type 'function)
                     'function
                     type)))
         (value (cond
                  ((sb-c::constant-lvar-p lvar)
                   (let ((value (sb-c::lvar-value lvar)))
                     (typecase value
                       ((cons sb-c::optional-dispatch)
                        :<optional-dispatch>)
                       ((cons sb-c::clambda)
                        (list :<lambda> (sb-c::lambda-%source-name (first value))))
                       (sb-c::vop-info
                        (sb-c::vop-info-name value))
                       (t
                        value)))))))
    (values kind leaf type value)))

(defmethod graph-object-node ((graph ir1-graph) (c sb-c::lvar))
  (when (showp graph :lvar)
    (multiple-value-bind (kind leaf type value) (lvar-info c)
      (let ((label (format nil "~A~@[ ~A~]~@[ : ~A~]~@[\\n = ~S~]"
                           kind leaf type  value)))
        (make-instance 'node :attributes `(:label     ,label
                                           :style     :filled
                                           :fillcolor "#ffcc99"
                                           :shape     :hexagon))))))

(defmethod graph-object-points-to ((graph ir1-graph) (c sb-c::lvar))
  (let ((dest (sb-c::lvar-dest c)))
    (mapcar (lambda (o) (attributify-edge o :color "brown" :weight 0))
            (list-no-nil dest)))        ; TODO color
  #+nil
  (cond
    ((typep dest 'sb-c::basic-combination)
     (if (member c (sb-c::basic-combination-args dest))
         nil
         (list-no-nil (sb-c::lvar-dest c))))
    (t (list-no-nil (sb-c::lvar-dest c))))
  )

(defmethod graph-object-pointed-to-by ((graph ir1-graph) (c sb-c::lvar))
  (let ((uses (sb-c::lvar-uses c)))
    (remove-if (lambda (x) (typep x 'sb-c::basic-combination))
               (if (listp uses)
                   uses
                   (list-no-nil uses)))))

;;;

(defun graph-component (component i &key show output-filename title)
  (let ((graph (generate-graph-from-roots
                (make-instance 'ir1-graph :show show)
                (list component)
                `(:compound t ; TODO automate compound in cl-dot?
                  :rankdir  "TB"
                  ,@(when title (list :label title)))))
        (output-filename (etypecase output-filename
                           (function (funcall output-filename i)))))
    (ensure-directories-exist output-filename)
    ; (cl-dot:print-graph graph :stream *error-output*)
    (cl-dot:dot-graph graph output-filename :format :png)))

(defun %graph-component (component i &key title)
  (block nil
    (handler-bind
        ((error (lambda (c)
                  (princ c)
                  (terpri)
                  (sb-debug:print-backtrace)
                  (break)
                  (return-from nil))))
      (graph-component component i :title title))))

(defvar *i*)
(defvar *component*)

(defun compile-with-phase-graphs
    (name definition
     &key
     (show            '(:lambda))
     (output-filename (lambda (i)
                        (format nil "/tmp/graph-~D.png" i))))
  (let ((*i* 0))
    (unwind-protect
         (let ((*component* nil))
           (trace sb-c::compile-component
                  :report nil
                  :print  (progn
                            (setf *component* (sb-debug:arg 0))
                            (values)))
           (macrolet ((trace-phase (function)
                        `(eval
                          (print (sb-debug::expand-trace
                            (list
                             ',function
                             :report      nil
                             :print-after `(block nil
                                             (when (or (equal (sb-c::component-name *component*)
                                                              '(lambda (&rest sb-debug::args)))
                                                       (equal (sb-c::component-name *component*)
                                                              '(lambda ())))
                                               (return (values)))
                                             (graph-component
                                              *component* (incf *i*)
                                              :show            (quote ,,'show)
                                              :output-filename ,,'output-filename
                                              :title           ,,(format nil "after ~A" function))
                                             (list *i* (sb-c::component-name *component*) ',',function))))))))
             (trace-phase sb-c::record-component-xrefs)
             (trace-phase sb-c::ir1-optimize)
             (trace-phase sb-c::constraint-propagate)
             (trace-phase sb-c::generate-type-checks)
             (trace-phase sb-c::ir1-finalize)
             (trace-phase sb-c::eliminate-dead-code)
             (trace-phase sb-c::physenv-analyze)
             (trace-phase sb-c::delete-if-no-entries)
             (trace-phase sb-c::%compile-component)
             (compile name definition)))
      (untrace))))


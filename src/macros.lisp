(in-package :tpapp-utils)

(defmacro with-parametrized-formulas ((&rest parameters)
                                      (&rest name-body-pairs)
                                      &body body)
  "Sequentially define (function-name (parameters) body) functions,
and have the a symbol-macro name expand to (function-name parameters).
Function-name is a fresh symbol.  Example:

  (with-parametrized-formulas (a b)
    ((f (+ a b))
     (g (+ f (* a b))))
    (g 3 7)) ; => 31 (which is 3+7+3*7)"
  (assert (and (every #'symbolp parameters)
               (every (lambda (name-body-pair)
                        (symbolp (car name-body-pair))) name-body-pairs)))
  (labels ((rec (name-body-pairs)
             (let* ((name (first (car name-body-pairs)))
                    (function-name name)
                    (function-body (second (car name-body-pairs)))
                    (rest (cdr name-body-pairs)))
               `(flet ((,function-name (,@parameters)
                         ,function-body))
                  (symbol-macrolet ((,name (,function-name ,@parameters)))
                    ,@(if rest
                          (list (rec rest)) ; ugly? workaround?
                          body))))))
    (rec name-body-pairs)))

(defmacro nil-on-errors ((&rest nil-conditions) &body body)
  "Body evaluates to nil if one of the conditions are signalled.  If
nil-conditions is empty, it is replaced by t."
  `(handler-case (progn
		   ,@body)
     (t (condition)
       (if ,(if (null nil-conditions)
		t
		`(member condition ',nil-conditions
		  :test (function typep)))
	   nil
	   (error condition)))))

(defmacro fn (&body body)
  "Macro for defining closures.  Usage:
    (fn form) => (lambda () form)
    (fn arg ...) => (lambda (arg) ...)
    (fn (arg+) ...) => (lambda (arg+) ...)"
  (cond
    ((not (cdr body))
     `(lambda () ,@body))
    (body
     (let ((args (car body)))
       `(lambda ,(mklist args)
          ,@body)))
    (t (error "Missing body."))))

(defmacro defp (variables values-form)
  "Like defparameter, but accepting lists (a b c) and multiple
values (:values a b c).  NIL's are not saved."
  (flet ((defparameter-expansion (vars vals)
             (mapcar (lambda (var val) 
                       (if var
                           `(defparameter ,var ,val)
                           nil))
                     vars vals)))
    (cond
      ((atom variables)
       `(defparameter ,variables ,values-form))
      ((eq (car variables) :values)
       (let* ((variables (cdr variables))
              (temporary-variables (mapcar #'gensym* variables)))
         (assert (every #'symbolp variables))
         `(multiple-value-bind ,temporary-variables ,values-form
            ,@(defparameter-expansion variables temporary-variables))))
      (t 
       (let ((temporary-variables (mapcar #'gensym* variables)))
         (assert (every #'symbolp variables))
         `(destructuring-bind ,temporary-variables ,values-form
            ,@(defparameter-expansion variables temporary-variables)))))))

(defmacro setf-template (template &body index-value-pairs)
  "For each INDEX and VALUE, replace _ in form with INDEX and assign
VALUE to that form using SETF.  Example:
 (setf-at-indexes (aref array _)
                  1 'a
                  3 'b)"
  `(setf ,@(iter
             (for (index value) :on index-value-pairs
                  :by #'strict-cddr)
             (collect (subst index '_ template))
             (collect value))))

(defmacro define-with-multiple-bindings (macro)
  "Define a version of `macro' with multiple arguments, given as a
list.  Application of `macro' will be nested.  The new name is the 
plural of the old one (generated using format)."
  (let ((plural (intern (format nil "~aS" macro))))
    `(defmacro ,plural (bindings &body body)
       ,(format nil "Multiple binding version of ~(~a~)." macro)
       (if bindings
	   `(,',macro ,(car bindings)
		     (,',plural ,(cdr bindings)
			       ,@body))
	   `(progn ,@body)))))


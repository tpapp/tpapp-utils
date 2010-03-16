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

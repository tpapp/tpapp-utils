(defpackage :tpapp-utils
    (:use :common-lisp :bind :iterate)
  (:export 

   ;; symbols

   make-symbol*
   
   ;; macros

   with-parametrized-formulas nil-on-errors

   ;; lists

   mklist has-duplicates? group

   ;; clos

   define-abstract-class
   
   ))


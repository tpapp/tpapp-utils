(defpackage :tpapp-utils
    (:use :common-lisp :bind :iterate :anaphora)
  (:export 

   ;; symbols

   concatenate-as-strings make-symbol-in make-symbol* make-keyword
   gensym* define-make-symbol% 
   
   ;; lists

   mklist has-duplicates? group all-equal? listn strict-cddr

   ;; macros

   with-parametrized-formulas nil-on-errors fn defp setf-template _
   define-with-multiple-bindings

   ;; clos

   define-abstract-class

   ;; misc
   
   silent

   ))


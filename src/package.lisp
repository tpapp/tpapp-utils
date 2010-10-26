(defpackage :tpapp-utils
  (:use :common-lisp :bind :iterate :anaphora :alexandria)
  (:export 

   ;; symbols

   concatenate-as-strings make-symbol-in make-symbol* make-keyword*
   gensym* define-make-symbol% 
   
   ;; lists

   mklist has-duplicates? group-list all-equal? listn strict-cddr

   ;; macros

   with-parametrized-formulas nil-on-errors fn defp setf-template _
   define-with-multiple-bindings use-locally

   ;; clos

   define-abstract-class

   ;; misc
   
   silent check-types

   ))


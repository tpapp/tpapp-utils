;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:tpapp-utils)

(defun concatenate-as-strings (args)
  (apply #'concatenate 'string (mapcar #'string args)))

(defun make-symbol-in (package &rest args)
  "Build a symbol by concatenating each element of ARGS as strings,
  and intern it in PACKAGE."
  (intern (concatenate-as-strings args) package))

(defun make-symbol* (&rest args)
  "Build a symbol by concatenating each element of ARGS as strings,
  and intern it (in *PACKAGE*, INTERN's default)."
  (apply #'make-symbol-in *package* args))

(defun make-keyword (&rest args)
  "Build a symbol by concatenating each element of ARGS as strings,
  and intern it the KEYWORD package."
  (apply #'make-symbol-in (load-time-value (find-package :keyword)) args))

(defun gensym* (&rest args)
  "Gensym with concatenating each element of ARGS as strings."
  (gensym (concatenate-as-strings args)))

(defmacro define-make-symbol% (package &optional
                               (name (make-symbol-in package '#:make-symbol%)))
  "Define a MAKE-SYMBOL% that interns in PACKAGE."
  `(defun ,name (&rest args) 
     ,(format nil "Build a symbol by concatenating each element of ~
                   ARGS as strings, and intern it in ~A." package)
     (intern (concatenate-as-strings args) ,package)))

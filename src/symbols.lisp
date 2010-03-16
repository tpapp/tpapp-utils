;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package #:tpapp-utils)

(defun make-symbol* (&rest args)
  "Build a symbol by concatenating each element of ARGS, and intern it
  (in *PACKAGE*, INTERN's default).  Arguments can be strings or
  symbols."
  (intern (apply #'concatenate 'string (mapcar #'string args))))


;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:tpapp-utils)

(defun silent (&rest arguments)
  "Make arguments vanish.  Used to avoid large displays/their cost in
benchmarking."
  (declare (ignore arguments))
  (values))

(defmacro check-types (type &rest arguments)
  "CHECK-TYPE for multiple places of the same type.  Each argument is either a
place, or a list of a place and a type-string."
  `(progn
     ,@(iter
         (for argument :in arguments)
         (collecting (if (atom argument)
                         `(check-type ,argument ,type)
                         (bind (((place type-string) argument))
                           `(check-type ,place ,type ,type-string)))))))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:tpapp-utils)

(defun silent (&rest arguments)
  "Make arguments vanish.  Used to avoid large displays/their cost in
benchmarking."
  (declare (ignore arguments))
  (values))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:tpapp-utils)

#-tpapp-utils-force
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library is deprecated and is no longer developed.  Push
  :TPAPP-UTILS-FORCE to *FEATURES* if you still want to use it.  Some features already
  exist in other libraries (like ALEXANDRIA) or were moved to CL-NUM-UTILS, LLA,
  CL-RANDOM, ..."))

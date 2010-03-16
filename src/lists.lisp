;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:tpapp-utils)

(defun mklist (atom-or-list)
  "If argument is not already a list, put it in one."
  (if (atom atom-or-list)
      (list atom-or-list)
      atom-or-list))

(defun has-duplicates? (list &key (key #'identity) (test #'eql))
  "Look for duplicate elements in a list using KEY and TEST.  Return
the first duplicate element, otherwise NIL.  Note: NILs do not count
as duplicates, even when there is many of them."
  (let (unique-keys)
    (dolist (element list)
      (let ((key (funcall key element)))
        (if (find key unique-keys :test test)
            (return-from has-duplicates? element)
            (push key unique-keys)))))
  nil)

(defun group (list n &aux (orig-list list))
  "Return elements of LIST as a list of lists in groups of N."
  ;; Thanks to Rainer Joswig on comp.lang.lisp for this version
  (check-type n (integer 1))
  (loop while list collect
        (loop repeat n
              unless list
              do (error "~A could not be broken up to sublists of ~A elements" orig-list n)
              collect (pop list))))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:tpapp-utils)

(defun mklist (atom-or-list &optional list-from-nil?)
  "If argument is not already a list, put it in one.  LIST-FROM-NIL
determines what happens to NIL."
  (typecase atom-or-list
    (null (if list-from-nil? (list nil) nil))
    (atom (list atom-or-list))
    (otherwise atom-or-list)))

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

(defun group-list (list n &aux (orig-list list))
  "Return elements of LIST as a list of lists in groups of N."
  ;; Thanks to Rainer Joswig on comp.lang.lisp for this version
  (check-type n (integer 1))
  (loop while list collect
        (loop repeat n
              unless list
              do (error "~A could not be broken up to sublists of ~A elements" orig-list n)
              collect (pop list))))

(defun all-equal? (test list)
  "Return non-nil IFF all items in LIST are equal to the first, using
TEST for comparison.  Empty and one-elements lists always result in
T."
  (unless list
    (return-from all-equal? t))
  (bind (((first &rest rest) list))
    (iter
      (for item :in rest)
      (always (funcall test first item)))))

(defun listn (n &rest items)
  "Return a list containing N of ITEMS."
  (iter
    (repeat n)
    (dolist (item items)
      (collecting item))))

(defun strict-cddr (list)
  "Return cddr if cdr exists, signal an error otherwise.  Used for
traversing a flat list of pairs."
  (aif (cdr list)
       (cdr it)
       (error "list ~A has a single element" list)))

;;; Copyright Tamas Papp 2010.

;;; Distributed under the Boost Software License, Version 1.0.  (See
;;; accompanying file LICENSE_1_0.txt or copy at
;;; http://www.boost.org/LICENSE_1_0.txt)
;;;
;;; This copyright notice pertains to all files in this library.

(defsystem tpapp-utils
  :description "Miscellaneous utility functions/macros I use frequently"
  :version "0.1"
  :author "Tamas K Papp"
  :license "Boost Software License - Version 1.0"
  :serial t
  :components 
  ((:module 
    "package-init"
    :pathname #P "src/"
    :components
    ((:file "package")))
   (:module
    "main"
    :pathname #P "src/"
    :serial t
    :components
    ((:file "lists")
     (:file "symbols")
     (:file "macros")
     (:file "clos")
     (:file "misc")))
   ;;  (:file "statistics")
   )
  :depends-on (:metabang-bind :iterate :cl-utilities :anaphora))


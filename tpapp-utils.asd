(defsystem tpapp-utils
  :description "Miscellaneous utility functions/macros I use frequently"
  :version "0.1"
  :author "Tamas K Papp"
  :license "GPL"
  :serial t
  :components 
  ((:module 
    "package-init"
    :pathname #P "src/"
    :components
    ((:file "package")))
   (:module
    "macros"
    :pathname #P "src/"
    :serial t
    :components
    ((:file "symbols")
     (:file "macros")))
   (:module
    "sequences"
    :pathname #P "src/"
    :serial t
    :components
    ((:file "lists")))
   (:module
    "clos"
    :pathname #P "src/"
    :serial t
    :components
    ((:file "clos")))
   ;;  (:file "statistics")
   )
  :depends-on (:metabang-bind :iterate :cl-utilities :anaphora))


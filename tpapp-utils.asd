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


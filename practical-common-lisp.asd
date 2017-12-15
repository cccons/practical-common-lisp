(asdf:defsystem #:practical-common-lisp
  :description "Implementation of projects from the Practical Common Lisp book by Peter Seibel"
  :author "Cameron V Chaparro <cccons@tuta.io>"
  :license "GPLv3"
  :serial t
  :pathname "src/"
  :components ((:module "ch03"
                :components
                ((:file "ch03")))
               (:module "ch08"
                :components
                ((:file "ch08")))
               (:module "ch09"
                :components
                ((:file "ch09")))
               (:module "ch15"
                :components
                ((:file "ch15")))))

(in-package #:cl-user)


(asdf:defsystem cl-htm
  :name "cl-htm"
  :version "0.0.0"
  :license "BSD simplified"
  :author "Marek Kochanowicz"
  :depends-on ( :iterate :vector-classes
                :serapeum :prove
                :cl-data-structures :alexandria
                :lparallel :metabang-bind)
  :defsystem-depends-on (:prove-asdf)
  :serial T
  :pathname "src"
  :components ((:file "aux-package")
               (:file "package")
               (:module "sdr"
                :components ((:file "package")
                             (:file "protocol")
                             (:file "types")))
               (:module "neuron-layer"
                :components ((:file "package")))))

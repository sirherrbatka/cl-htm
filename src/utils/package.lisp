(in-package #:cl-user)


(defpackage cl-htm.utils
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:random-float
   #:random-synapses-strength
   #:matching
   #:at
   #:make-lazy-vector
   #:vector-iota))

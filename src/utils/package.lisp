(in-package #:cl-user)


(defpackage cl-htm.utils
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:at
   #:content
   #:size
   #:make-lazy-vector
   #:matching
   #:random-float
   #:random-synapses-strength
   #:vector-iota))

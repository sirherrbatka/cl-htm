(in-package #:cl-htm.utils)


(defun random-float ()
  (/ (the fixnum (random most-positive-fixnum))
     #.(/ most-positive-fixnum 1.0)))

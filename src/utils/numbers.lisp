(in-package #:cl-htm.utils)


(defun random-float ()
  (/ (the fixnum (random most-positive-fixnum))
     #.(/ most-positive-fixnum 1.0)))

(-> vector-iota (fixnum fixnum) (simple-array fixnum))
(defun vector-iota (from to)
  (let* ((size (- to from))
         (result (make-array size :element-type 'fixnum)))
    (iterate
      (for i from 0)
      (for v from from below to)
      (setf (aref result i) v))
    result))

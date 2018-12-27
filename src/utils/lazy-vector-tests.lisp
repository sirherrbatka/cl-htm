(in-package #:cl-htm.utils)

(prove:plan 7)

(let ((vector (make-lazy-vector t
                                (cl-ds:iota-range :from 1 :to 6))))
  (prove:is (matching vector #'oddp) 1)
  (iterate
    (for i from 0 below 5)
    (prove:is (at vector i) (1+ i)))
  (prove:is (type-of vector) 'lazy-vector))

(prove:finalize)

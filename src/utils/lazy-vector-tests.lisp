(in-package #:cl-htm.utils)

(prove:plan 7)

(let ((vector (make-lazy-vector t
                                (cl-ds:iota-range :from 0 :to 5))))
  (prove:is (matching vector #'oddp) 1)
  (iterate
    (for i from 0 below 5)
    (prove:is (at vector i) i))
  (prove:is (type-of vector) 'lazy-vector))

(prove:finalize)

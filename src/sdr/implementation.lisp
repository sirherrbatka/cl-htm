(in-package #:cl-htm.sdr)


(defmethod clear-active ((sdr sdr))
  (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
    (iterate
      (for i from 0 below (vector-classes:size sdr))
      (setf (active-neuron) 0)
      (finally (return sdr)))))

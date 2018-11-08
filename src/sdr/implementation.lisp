(in-package #:cl-htm.sdr)


(defmethod clear-active ((sdr sdr))
  (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
    (iterate
      (for i from 0 below (vector-classes:size sdr))
      (setf (active-neuron) 0)
      (finally (return sdr)))))


(defmethod set-active ((sdr sdr) input)
  (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
    (iterate
      (for i in-vector input)
      (setf (active-neuron) 1)
      (finally (return sdr)))))


(defmethod select-active ((sdr sdr) &optional destination)
  (lret ((destination (or destination
                          (make-array (truncate (vector-classes:size sdr) 40)
                                      :fill-pointer 0
                                      :adjustable 0))))
    (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
      (iterate
        (for i from 0 below (vector-classes:size sdr))
        (unless (zerop (active-neuron))
          (vector-push-extend i destination))))))

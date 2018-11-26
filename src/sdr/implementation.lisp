(in-package #:cl-htm.sdr)


(defmethod clear-all-active ((sdr sdr))
  (declare (optimize (speed 3) (safety 0)))
  (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
    (iterate
      (declare (type fixnum i))
      (for i from 0 below (the fixnum (vector-classes:size sdr)))
      (setf (active-neuron) 0)
      (finally (return sdr)))))


(defmethod set-active ((sdr sdr) input value)
  (check-type value bit)
  (check-type input vector)
  (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
    (iterate
      (for i in-vector input)
      (setf (active-neuron) value)
      (finally (return sdr)))))


(defmethod select-active ((sdr sdr) value &optional destination)
  (check-type value bit)
  (check-type destination (or null vector))
  (lret ((destination (or destination
                          (make-array (truncate (vector-classes:size sdr) 40)
                                      :fill-pointer 0
                                      :adjustable 0))))
    (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
      (iterate
        (for i from 0 below (vector-classes:size sdr))
        (when (eql (active-neuron) value)
          (vector-push-extend i destination))))))


(defmethod clone ((sdr sdr))
  (lret ((result (vector-classes:make-data (type-of sdr)
                                           (vector-classes:size sdr))))
    (map-into (read-active-neurons result)
              #'identity
              (read-active-neurons sdr))))

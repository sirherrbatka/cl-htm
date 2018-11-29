(in-package #:cl-htm.sdr)


(defmethod clear-all-active ((sdr sdr))
  (declare (optimize (speed 3) (safety 0)))
  (let ((active (dense-active-neurons sdr)))
    (declare (type (or null (array fixnum (*)))
                   active))
    (vector-classes:with-data (((active-neuron active-neurons)) sdr i sdr)
      (if (null active)
          (iterate
            (declare (type fixnum i))
            (for i from 0 below (the fixnum (vector-classes:size sdr)))
            (setf (active-neuron) 0))
          (iterate
            (declare (type fixnum i k length))
            (with length = (length active))
            (for k from 0 below length)
            (for i = (aref active k))
            (setf (active-neuron) 0)))))
  sdr)


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

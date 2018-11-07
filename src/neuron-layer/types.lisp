(in-package #:cl-htm.nl)


(vector-classes:define-data neuron-column ()
  ((input :array t
          :type non-negative-fixnum
          :reader read-input
          :dimensions-arg :input-size)))


(vector-classes:define-data neuron-layer (cl-htm.sdr:sdr)
  ((%columns :initarg :columns
             :type neuron-column
             :reader read-columns)
   (%threshold :initarg :threshold
               :reader read-threshold
               :type single-float)
   (predictive-neurons :array t
                       :initform 0
                       :reader read-predictive-neurons
                       :type bit)
   (synapses-strength :array t
                      :type single-float
                      :dimensions-arg :synapses-strength
                      :initform (cl-htm.utils:random-float))))

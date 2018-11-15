(in-package #:cl-htm.nl)


(vector-classes:define-data neuron-column ()
  ((input :array t
          :type non-negative-fixnum
          :reader read-input
          :documentation "Indices of neurons (of input SDR) connected to this column."
          :dimensions-arg :input-size)))


(defclass fundamental-training-parameters ()
  ())


(defclass basic-training-parameters ()
  ((%p+ :initarg :p+
        :documentation "Used to increase active synapses strength."
        :type single-float
        :reader p+)
   (%p- :initarg :p-
        :documentation "Used to decrease inactive synapses strength."
        :type single-float
        :reader p-)
   (%decay :initarg :decay
           :type single-float
           :reader decay)
   (%threshold :initarg :threshold
               :reader threshold
               :type single-float
               :documentation "Threshold for predictive state of neuron.")
   (%activated-columns-count :initarg :activated-columns-count
                             :reader activated-columns-count
                             :type positive-fixnum
                             :documentation "How many columns should become active?")))


(vector-classes:define-data neuron-layer (cl-htm.sdr:sdr)
  ((%column-indices :initarg :column-indices
                    :type (simple-array fixnum)
                    :reader read-column-indices)
   (%threshold :initarg :threshold
               :reader read-threshold
               :type single-float
               :documentation "Threshold for predictive state of neuron.")
   (%activated-columns-count :initarg :activated-columns-count
                             :reader read-activated-columns-count
                             :type positive-fixnum
                             :documentation "How many columns should become active?")
   (synapses-strength :array t
                      :type single-float
                      :dimensions-arg :synapses-strength
                      :documentation "Weights of synapses for each neuron."
                      :initform (cl-htm.utils:random-float))))

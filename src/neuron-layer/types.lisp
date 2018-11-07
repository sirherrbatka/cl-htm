(in-package #:cl-htm.nl)


(vector-classes:define-data neuron-column ()
  ((%activated-neurons-count :initarg :activated-neurons-count
                             :reader read-activated-neurons-count
                             :documentation "How many neurons should become active?"
                             :type positive-fixnum)
   (input :array t
          :type non-negative-fixnum
          :reader read-input
          :documentation "Indices of neurons (of input SDR) connected to this column."
          :dimensions-arg :input-size)))


(vector-classes:define-data neuron-layer (cl-htm.sdr:sdr)
  ((%columns :initarg :columns
             :type neuron-column
             :reader read-columns
             :documentation "All columns of this layer.")
   (%p+ :initarg :p+
        :documentation "Used to increase active synapses strength."
        :type single-float
        :reader read-p+)
   (%p- :initarg :p-
        :documentation "Used to decrease inactive synapses strength."
        :type single-float
        :reader read-p-)
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

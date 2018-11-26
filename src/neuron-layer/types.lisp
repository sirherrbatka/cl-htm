(in-package #:cl-htm.nl)


(vector-classes:define-data neuron-column ()
  ((input :array t
          :type non-negative-fixnum
          :reader read-input
          :documentation "Indices of neurons (of input SDR) connected to this column."
          :dimensions-arg :input-size)
   (%column-indices :initarg :column-indices
                    :type (simple-array fixnum)
                    :reader read-column-indices)))


(vector-classes:define-data neuron-layer-weights ()
  ((%columns :initarg :columns
             :reader columns)
   (synapses-strength :array t
                      :type single-float
                      :dimensions-arg :synapses-strength
                      :documentation "Weights of synapses for each neuron."
                      :initform (cl-htm.utils:random-float))))


(vector-classes:define-data neuron-layer (neuron-layer-weights)
  ())


(defclass layer ()
  ((%input-size :initarg :input-size
                :initform nil
                :accessor access-input-size)
   (%type :initarg :type
          :reader read-type)
   (%arguments :initarg :arguments
               :reader read-arguments)))

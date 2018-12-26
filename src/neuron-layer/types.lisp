(in-package #:cl-htm.nl)


(vector-classes:define-data neuron-column ()
  ((input :array t
          :type non-negative-fixnum
          :reader read-input
          :documentation "Indices of neurons (of input SDR) connected to this column."
          :dimensions-arg :input-size)
   (locks :array t
          :initform (bt:make-lock)
          :type bt:lock
          :reader read-locks)
   (%column-indices :initarg :column-indices
                    :type (simple-array fixnum)
                    :reader read-column-indices)))


(vector-classes:define-data neuron-layer-weights ()
  ((%columns :initarg :columns
             :reader columns)
   (distal-segments
    :array t
    :type t
    :initform nil
    :documentation "Weights and locations of the distal segments (lazily initialized).")
   (%segments-count
    :array nil
    :type fixnum
    :accessor access-segments-count
    :initarg :segments-count)
   (%synapses-count
    :array nil
    :accessor access-synapses-count
    :type fixnum
    :initarg :synapses-count)
   (%input-size :initarg :input-size
                :accessor access-input-size)
   (proximal-synapses-strength
    :array t
    :type non-negative-fixnum
    :dimensions-arg :proximal-synapses-count
    :documentation "Weights of proximal synapses for each neuron."
    :initform (cl-htm.utils:random-synapses-strength))))


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

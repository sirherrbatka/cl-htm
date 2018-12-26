(in-package #:cl-user)


(defpackage #:cl-htm.neuron-layer
  (:use #:cl #:cl-htm.aux-package)
  (:nicknames #:cl-htm.nl)
  (:export
   #:activate
   #:activated-columns-count
   #:calculate-active-synapses-for-columns
   #:columns
   #:context
   #:output-size
   #:layer
   #:neuron-layer-weights
   #:to-effective-layer
   #:make-weights
   #:to-declared-layer
   #:to-read-only-layer
   #:declared-layers
   #:layers
   #:layer
   #:neuron-column
   #:neuron-layer
   #:select-active-columns
   #:select-active-neurons
   #:select-predictive-neurons
   #:update-synapses))

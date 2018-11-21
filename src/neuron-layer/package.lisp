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
   #:layer
   #:neuron-layer-weights
   #:to-sdr
   #:make-weights
   #:effective-layer
   #:effective-layers
   #:layers
   #:layer
   #:neuron-column
   #:neuron-layer
   #:select-active-columns
   #:select-active-neurons
   #:select-predictive-neurons
   #:update-synapses))

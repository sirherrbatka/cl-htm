(in-package #:cl-user)


(defpackage #:cl-htm.neuron-layer
  (:use #:cl #:cl-htm.aux-package)
  (:nicknames #:cl-htm.nl)
  (:export
   #:fundamental-training-parameters
   #:basic-training-parameters
   #:neuron-layer
   #:neuron-column
   #:columns
   #:activate
   #:update-synapses
   #:select-predictive-neurons
   #:select-active-neurons
   #:select-active-columns
   #:calculate-active-synapses-for-columns
   #:p+
   #:p-
   #:decay-
   #:threshold
   #:activated-columns-count))

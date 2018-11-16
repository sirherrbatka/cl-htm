(in-package #:cl-user)


(defpackage #:cl-htm.neuron-layer
  (:use #:cl #:cl-htm.aux-package)
  (:nicknames #:cl-htm.nl)
  (:export
   #:activate
   #:activated-columns-count
   #:basic-training-context
   #:basic-training-parameters
   #:calculate-active-synapses-for-columns
   #:columns
   #:decay-
   #:fundamental-training-context
   #:fundamental-training-parameters
   #:neuron-column
   #:neuron-layer
   #:p+
   #:p-
   #:select-active-columns
   #:select-active-neurons
   #:select-predictive-neurons
   #:threshold
   #:update-synapses))

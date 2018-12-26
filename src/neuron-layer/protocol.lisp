(in-package #:cl-htm.nl)


(defgeneric activate (neuron-layer
                      sdr
                      context
                      training-parameters
                      mode))

(defgeneric update-synapses (training-parameters layer input mode
                             columns active-columns
                             predictive-neurons active-neurons))

(defgeneric select-predictive-neurons (layer sdr training-parameters
                                       columns active-columns
                                       context))

(defgeneric select-active-neurons (layer
                                   columns
                                   input
                                   active-columns
                                   predictive-neurons
                                   active-neurons))

(defgeneric select-active-columns (layer training-parameters
                                   columns active-synapses))

(defgeneric calculate-active-synapses-for-columns (layer input columns))

(defgeneric columns (layer))

(defgeneric context (layer))

(defgeneric to-effective-layer (neuron-layer))

(defgeneric layer (type &key))

(defgeneric make-weights (type input-size &key))

(defgeneric declared-layers (layers initial-size))

(defgeneric to-declared-layer (declared-layer prev-layer))

(defgeneric to-read-only-layer (layer))

(defun layers (&rest arguments)
  (apply #'list arguments))

(defgeneric output-size (layer))

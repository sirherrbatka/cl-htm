(in-package #:cl-htm.nl)


(defgeneric activate (neuron-layer
                      sdr
                      context
                      training-parameters))

(defgeneric update-synapses (training-parameters layer input
                             columns active-columns
                             predictive-neurons active-neurons))

(defgeneric select-predictive-neurons (layer sdr training-parameters
                                       columns active-columns))

(defgeneric select-active-neurons (layer columns
                                   active-columns
                                   input
                                   predictive-neurons))

(defgeneric select-active-columns (layer training-parameters
                                   columns active-synapses))

(defgeneric calculate-active-synapses-for-columns (layer input columns))

(defgeneric columns (layer))

(defgeneric context (layer))

(defgeneric to-sdr (neuron-layer))

(defgeneric layer (type &rest args))

(defgeneric make-weights (type input-size &key))

(defgeneric effective-layers (declared-layers))

(defgeneric effective-layer (declared-layer prev-layer))

(defun layers (&rest arguments)
  (apply #'list arguments))

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

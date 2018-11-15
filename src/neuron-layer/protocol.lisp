(in-package #:cl-htm.nl)


(defgeneric activate (neuron-layer sdr training-parameters
                      &optional prev-data))

(defgeneric update-synapses (layer input training-parameters
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

(defgeneric p+ (training-parameters))

(defgeneric p- (training-parameters))

(defgeneric decay (training-parameters))

(defgeneric threshold (training-parameters))

(defgeneric activeted-columns-count (training-parameters))

(defgeneric columns (layer))

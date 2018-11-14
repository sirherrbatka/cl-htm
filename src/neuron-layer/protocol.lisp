(in-package #:cl-htm.nl)


(defgeneric activate (neuron-layer sdr context &optional prev-data))

(defgeneric update-synapses (layer columns
                             predictive-neurons active-neurons))

(defgeneric select-predictive-neurons (layer sdr columns active-columns))

(defgeneric select-active-neurons (layer columns
                                   active-columns
                                   input
                                   predictive-neurons))

(defgeneric select-active-columns (layer columns active-synapses))

(defgeneric calculate-active-synapses-for-columns (layer columns input))

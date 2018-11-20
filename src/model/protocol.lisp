(in-package #:cl-htm.model)


(defgeneric encode-data-point (input destination data-point))

(defgeneric more-data-p (input data-point))

(defgeneric reset-model (model contexts sdrs))

(defgeneric activate (model mode contexts parameters sdrs))

(defgeneric sdrs (model))

(defgeneric parameters (model mode))

(defgeneric context (layer model))

(defgeneric contexts (model))

(defgeneric output-sdr (model sdrs))

(defgeneric input-sdr (model sdrs))

(defgeneric decode-sdr (decoder sdr))

(defgeneric pass-to-decoder (decoder model mode data-point sdrs))

(defgeneric insert-point (input decoder model mode data-point contexts sdrs))

(defgeneric predict (input decoder model data))

(defgeneric train (input decoder model data))

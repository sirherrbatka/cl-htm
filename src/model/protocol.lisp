(in-package #:cl-htm.model)


(defgeneric encode-data-point (input destination data-point))

(defgeneric more-data-p (input mode data-point))

(defgeneric reset-model (model sdrs contexts))

(defgeneric activate (model mode contexts
                      parameters sdrs))

(defgeneric layers (model))

(defgeneric parameters (model))

(defgeneric context (layer model))

(defgeneric contexts (model))

(defgeneric input/output-sdr (model sdrs))

(defgeneric decode-sdr (decoder sdr))

(defgeneric pass-to-decoder (decoder model mode
                             data-point sdrs contexts))

(defgeneric insert-point (input decoder model mode
                          data-point contexts sdrs))

(defgeneric predict (model data &key input decoder))

(defgeneric train (model data &key input decoder))

(defgeneric adapt (model data &key input decoder))

(defgeneric make-model (model-class
                        input-size
                        training-parameters
                        layers
                        &key input decoder))

(defgeneric input (model))

(defgeneric (setf input) (value model))

(defgeneric decoder (model))

(defgeneric (setf decoder) (value model))

(defgeneric hashes (encoder))

(defgeneric hash-function (encoder))

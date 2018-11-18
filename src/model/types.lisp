(in-package #:cl-htm.model)


(defclass fundamental-input ()
  ())


(defclass sequence-input (fundamental-input)
  ())


(defclass fundamental-model ()
  ())


(defclass basic-model (fundamental-model)
  ((%layers
    :initarg :layers
    :type list
    :reader read-layers)
   (%input-sdrs
    :initarg :input-sdr
    :reader input-sdrs)
   (%output-sdrs
    :initarg :output-sdrs
    :reader read-output-sdrs
    :reader output-sdrs)
   (%training-parameters
    :initarg :training-parameters
    :reader read-training-parameters
    :type cl-htm.training:fundamental-training-parameters)
   (%context
    :initarg :context
    :reader read-context
    :type cl-htm.training:fundamental-training-context))
  (:default-initargs :context (make 'cl-htm.training:basic-training-context)))


(defclass fundamental-encoder ()
  ())


(defclass fundamental-decoder ()
  ())


(defclass fundamental-discreete-decoder (fundamental-decoder)
  ())


(defclass fundamental-mode ()
  ())


(defclass train-mode (fundamental-mode)
  ())


(defclass predict-mode (fundamental-mode)
  ())


(defclass adapt-mode (predict-mode)
  ())

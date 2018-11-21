(in-package #:cl-htm.model)


(defclass fundamental-input ()
  ())


(defclass fundamental-model ()
  ())


(defclass basic-model (fundamental-model)
  ((%layers
    :initarg :layers
    :type list
    :reader read-layers)
   (%input-sdr-size
    :initarg :input-sdr-size
    :reader read-input-sdr-size)
   (%training-parameters
    :initarg :training-parameters
    :reader read-training-parameters
    :type cl-htm.training:fundamental-training-parameters)))


(defclass fundamental-encoder (fundamental-input)
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


(defclass random-symbol-encoder (fundamental-encoder) ;or atom-encoder?
  ((%hashes :initarg :hashes
            :type simple-array
            :reader read-hashes)
   (%count :initarg :count
           :type non-negative-fixnum
           :reader read-count)
   (%hash-function :initarg :hash-function
                   :reader read-hash-function
                   :initform #'sxhash)))


(defclass random-vector-encoder (random-symbol-encoder)
  ((%encoded-length :initarg :encoded-duration
                    :reader read-encoded-length)))

(in-package #:cl-user)


(defpackage #:cl-htm.model
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activate
   #:adapt-mode
   #:basic-model
   #:context
   #:contexts
   #:decode-sdr
   #:encode-data-point
   #:fundamental-decoder
   #:fundamental-discreete-decoder
   #:fundamental-encoder
   #:fundamental-input
   #:fundamental-mode
   #:fundamental-model
   #:input-sdr
   #:input
   #:decoder
   #:insert-point
   #:make-model
   #:more-data-p
   #:output-sdr
   #:parameters
   #:pass-to-decoder
   #:predict
   #:predict-mode
   #:random-symbol-encoder
   #:random-vector-encoder
   #:reset-model
   #:sdrs
   #:sequence-input
   #:train
   #:train-mode))

(in-package #:cl-user)


(defpackage #:cl-htm.model
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activate
   #:adapt
   #:basic-model
   #:context
   #:contexts
   #:decode-sdr
   #:encode-data-point
   #:fundamental-decoder
   #:fundamental-discreete-decoder
   #:fundamental-encoder
   #:fundamental-input
   #:fundamental-model
   #:input
   #:output-size
   #:decoder
   #:insert-point
   #:make-model
   #:make-vector-decoder
   #:more-data-p
   #:input/output-sdr
   #:parameters
   #:pass-to-decoder
   #:predict
   #:random-symbol-encoder
   #:random-vector-encoder
   #:reset-model
   #:layers
   #:sequence-input
   #:train))

(in-package #:cl-user)


(defpackage #:cl-htm.model
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activate
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
   #:random-symbol-encoder
   #:random-vector-encoder
   #:reset-model
   #:sdrs
   #:sequence-input
   #:train))

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
   #:decoder
   #:encode-data-point
   #:fundamental-decoder
   #:fundamental-discreete-decoder
   #:fundamental-encoder
   #:fundamental-input
   #:fundamental-model
   #:hash-function
   #:hashes
   #:hashval
   #:input
   #:input/output-sdr
   #:insert-point
   #:ensure-data-wrapping
   #:encoded-length
   #:layers
   #:make-model
   #:make-vector-decoder
   #:more-data-p
   #:output-size
   #:parameters
   #:pass-to-decoder
   #:predict
   #:random-symbol-encoder
   #:random-vector-encoder
   #:reset-model
   #:sequence-input
   #:train))

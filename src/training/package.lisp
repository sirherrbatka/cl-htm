(in-package #:cl-user)


(defpackage #:cl-htm.training
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activated-columns-fraction
   #:adapt-mode
   #:basic-context
   #:basic-parameters
   #:decay
   #:fundamental-context
   #:fundamental-mode
   #:fundamental-parameters
   #:maximum-synaps-weight
   #:p+
   #:p-
   #:past-predictive-neurons
   #:predict-mode
   #:reset-context
   #:threshold
   #:train-mode
   #:active-neurons))

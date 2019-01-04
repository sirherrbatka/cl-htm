(in-package #:cl-user)


(defpackage #:cl-htm.training
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activated-columns-fraction
   #:adapt-mode
   #:active-neurons
   #:first-iteration
   #:basic-context
   #:basic-parameters
   #:decay
   #:fundamental-context
   #:fundamental-mode
   #:fundamental-parameters
   #:maximum-weight
   #:minimum-weight
   #:extra-data
   #:p+
   #:p-
   #:past-predictive-neurons
   #:predict-mode
   #:reset-context
   #:threshold
   #:train-mode
   #:active-neurons))

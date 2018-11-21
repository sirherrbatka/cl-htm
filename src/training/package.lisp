(in-package #:cl-user)


(defpackage #:cl-htm.training
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activated-columns-fraction
   #:reset-context
   #:basic-training-context
   #:basic-training-parameters
   #:empty-training-parameters
   #:decay
   #:active-neurons
   #:past-predictive-neurons
   #:fundamental-training-context
   #:fundamental-training-parameters
   #:p+
   #:p-
   #:threshold))

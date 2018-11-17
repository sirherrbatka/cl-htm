(in-package #:cl-user)


(defpackage #:cl-htm.training
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activated-columns-fraction
   #:basic-training-context
   #:basic-training-parameters
   #:empty-training-parameters
   #:decay
   #:past-predictive-neurons
   #:fundamental-training-context
   #:fundamental-training-parameters
   #:p+
   #:p-
   #:threshold))

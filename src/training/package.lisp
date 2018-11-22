(in-package #:cl-user)


(defpackage #:cl-htm.training
  (:use #:cl #:cl-htm.aux-package)
  (:export
   #:activated-columns-fraction
   #:reset-context
   #:basic-context
   #:basic-parameters
   #:decay
   #:active-neurons
   #:past-predictive-neurons
   #:fundamental-context
   #:fundamental-parameters
   #:p+
   #:p-
   #:threshold))

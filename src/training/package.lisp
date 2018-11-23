(in-package #:cl-user)


(defpackage #:cl-htm.training
  (:use #:cl #:cl-htm.aux-package)
  (:export
    #:active-neurons
    #:activated-columns-fraction
    #:basic-context
    #:basic-parameters
    #:decay
    #:fundamental-context
    #:fundamental-parameters
    #:p+
    #:p-
    #:past-predictive-neurons
    #:no-training-parameters
    #:reset-context
    #:threshold))

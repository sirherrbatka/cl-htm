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
    #:adapt-mode
    #:predict-mode
    #:train-mode
    #:fundamental-mode
    #:reset-context
    #:threshold))

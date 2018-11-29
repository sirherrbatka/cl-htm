(in-package #:cl-htm.sdr)


(vector-classes:define-data sdr ()
  ((active-neurons :array t
                   :initform 0
                   :reader read-active-neurons
                   :type bit)
   (%dense-active-neurons :array nil
                          :initform nil
                          :accessor dense-active-neurons
                          :type simple-vector)))

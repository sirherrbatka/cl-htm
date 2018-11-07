(in-package #:cl-htm.sdr)


(vector-classes:define-data sdr ()
  ((active-neurons :array t
                   :initform 0
                   :reader read-active-neurons
                   :type bit)))

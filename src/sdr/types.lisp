(in-package #:cl-htm.sdr)


(vector-classes:define-data sdr ()
  ((content :array t
            :initarg :content
            :reader read-content
            :type bit)))

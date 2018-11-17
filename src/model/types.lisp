(in-package #:cl-htm.model)


(defclass fundamental-input ()
  ())


(defclass sequence-input (fundamental-input)
  ())


(defclass fundamental-model ()
  ())


(defclass basic-model (fundamental-model)
  ((%sdrs :initarg :sdrs
          :type list
          :reader read-sdrs)
   (%input-sdrs :initarg :input-sdr
                :reader input-sdrs)))


(defclass fundamental-encoder ()
  ())


(defclass fundamental-decoder ()
  ())


(defclass fundamental-discreete-decoder (fundamental-decoder)
  ())

(in-package #:cl-htm.sdr)


(defgeneric clear-all-active (sdr))

(defgeneric set-active (sdr input value))

(defgeneric select-active (sdr value &optional destination))

(defgeneric clone (sdr))

(defgeneric dense-active-neurons (sdr))

(defgeneric (setf dense-active-neurons) (new-value sdr))

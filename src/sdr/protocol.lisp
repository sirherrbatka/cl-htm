(in-package #:cl-htm.sdr)


(defgeneric clear-all-active (sdr))

(defgeneric set-active (sdr input value))

(defgeneric select-active (sdr value &optional destination))

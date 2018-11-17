(in-package :cl-htm.training)


(defgeneric p+ (training-parameters))

(defgeneric p- (training-parameters))

(defgeneric decay (training-parameters))

(defgeneric threshold (training-parameters))

(defgeneric activeted-columns-fraction (training-parameters))

(defgeneric past-predictive-neurons (context))

(defgeneric (setf past-predictive-neurons) (new-value context))

(defgeneric reset-context (context))

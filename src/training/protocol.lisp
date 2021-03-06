(in-package :cl-htm.training)


(defgeneric p+ (parameters))

(defgeneric p- (parameters))

(defgeneric decay (parameters))

(defgeneric threshold (parameters))

(defgeneric activated-columns-fraction (parameters))

(defgeneric past-predictive-neurons (context))

(defgeneric (setf past-predictive-neurons) (new-value context))

(defgeneric reset-context (context))

(defgeneric maximum-weight (parameters))

(defgeneric minimum-weight (parameters))

(defgeneric extra-data (context location))

(defgeneric (setf extra-data) (new-val context location))

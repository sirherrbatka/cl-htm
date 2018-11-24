(in-package :cl-htm.training)


(define-constant +empty-vector+
    (make-array 0 :element-type 'non-negative-fixnum)
  :test #'vector=)


(defclass fundamental-parameters ()
  ())


(defclass fundamental-context ()
  ())


(defclass basic-context (fundamental-context)
  ((%past-predictive-neurons :initarg :past-predictive-neurons
                             :accessor past-predictive-neurons)
   (%active-neurons :initarg :active-neurons
                    :reader active-neurons))
  (:default-initargs
   :past-predictive-neurons +empty-vector+
   :active-neurons (make-array 0 :element-type 'non-negative-fixnum
                                 :fill-pointer 0
                                 :adjustable t)))


(defclass basic-parameters (fundamental-parameters)
  ((%p+
    :initarg :p+
    :documentation "Used to increase active synapses strength."
    :type single-float
    :reader p+)
   (%p-
    :initarg :p-
    :documentation "Used to decrease inactive synapses strength."
    :type single-float
    :reader p-)
   (%decay
    :initarg :decay
    :type single-float
    :reader decay)
   (%threshold
    :initarg :threshold
    :reader threshold
    :type single-float
    :documentation "Threshold for predictive state of neuron.")
   (%activated-columns-fraction
    :initarg :activated-columns-fraction
    :reader activated-columns-fraction
    :documentation "How many columns should become active?")))


(defclass fundamental-mode ()
  ())


(defclass train-mode (fundamental-mode)
  ())


(defclass predict-mode (fundamental-mode)
  ())


(defclass adapt-mode (predict-mode)
  ())

(in-package :cl-htm.training)


(define-constant +empty-vector+
    (make-array 0)
  :test #'vector=)


(defclass fundamental-parameters ()
  ())


(defclass fundamental-context ()
  ())


(defclass basic-context (fundamental-context)
  ((%past-predictive-neurons :initarg :past-predictive-neurons
                             :accessor past-predictive-neurons)
   (%active-neurons :initarg :active-neurons
                    :accessor active-neurons)
   (%first-iteration :initform t
                     :accessor first-iteration)
   (%extra-data :initarg :extra-data
                :reader read-extra-data))
  (:default-initargs
   :past-predictive-neurons +empty-vector+
   :extra-data (make-hash-table)
   :active-neurons (make-array 0 :element-type t
                                 :fill-pointer 0
                                 :adjustable t)))


(defclass basic-parameters (fundamental-parameters)
  ((%p+
    :initarg :p+
    :documentation "Used to increase active synapses strength."
    :type fixnum
    :reader p+)
   (%p-
    :initarg :p-
    :documentation "Used to decrease inactive synapses strength."
    :type fixnum
    :reader p-)
   (%decay
    :initarg :decay
    :type fixnum
    :reader decay)
   (%threshold
    :initarg :threshold
    :reader threshold
    :type fixnum
    :documentation "Threshold for predictive state of neuron.")
   (%maximum-weight
    :initarg :maximum-weight
    :reader maximum-weight
    :type fixnum)
   (%minimum-weight
    :initarg :minimum-weight
    :reader minimum-weight
    :type fixnum)
   (%activated-columns-fraction
    :initarg :activated-columns-fraction
    :reader activated-columns-fraction
    :documentation "How many columns should become active?"))
  (:default-initargs
   :minimum-weight 5
   :maximum-weight 100))


(defclass fundamental-mode ()
  ())


(defclass train-mode (fundamental-mode)
  ())


(defclass predict-mode (fundamental-mode)
  ())


(defclass adapt-mode (predict-mode)
  ())

(in-package :cl-htm.training)


(defclass fundamental-training-parameters ()
  ())


(defclass empty-training-parameters ()
  ())


(defclass fundamental-training-context ()
  ())


(defclass basic-training-context ()
  ((%part-predictive-neurons :initarg :part-predictive-neurons
                             :accessor past-predictive-neurons)))


(defclass basic-training-parameters ()
  ((%p+ :initarg :p+
        :documentation "Used to increase active synapses strength."
        :type single-float
        :reader p+)
   (%p- :initarg :p-
        :documentation "Used to decrease inactive synapses strength."
        :type single-float
        :reader p-)
   (%decay :initarg :decay
           :type single-float
           :reader decay)
   (%threshold :initarg :threshold
               :reader threshold
               :type single-float
               :documentation "Threshold for predictive state of neuron.")
   (%activated-columns-fraction :initarg :activated-columns-fraction
                                :reader activated-columns-fraction
                                :documentation "How many columns should become active?")))

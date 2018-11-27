(in-package #:cl-htm.model)


(defun jaccard-metric (vect1 vect2)
  cl-ds.utils:todo)


(defclass all-outputs ()
  ((%stored-outputs
    :initarg :stored-outputs
    :reader read-stored-outputs
    :documentation "Metric dictionary, mapping outputs to values.")
   (%buffer
    :initarg :buffer
    :initform (vect)
    :reader read-buffer
    :documentation "Temporary buffer for all outputs, before creating metric dictionary.")
   (%close-limit
    :initarg :close-limit
    :accessor access-close-limit
    :documentation "Limit for metric to trim search in the metric dictionary.")))


(defun clear-buffer (outputs)
  cl-ds.utils:todo)


(defun fill-dictionary (outputs)
  cl-ds.utils:todo
  (clear-buffer outputs)
  outputs)


(defun add-data-point (outputs contexts data-point)
  cl-ds.utils:todo)


(defun prediction (contexts output)
  cl-ds.utils:todo)

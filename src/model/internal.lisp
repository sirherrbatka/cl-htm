(in-package #:cl-htm.model)


(defun jaccard-metric (vect1 vect2)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0))
           (type (simple-array fixnum (*)) vect1 vect2))
  (let* ((union 0.0)
         (intersection 0.0)
         (increase-union (lambda (x)
                           (declare (ignore x))
                           (incf union))))
    (declare (type single-float union intersection))
    (cl-ds.utils:on-ordered-intersection
     (lambda (a b)
       (declare (ignore a b))
       (incf union)
       (incf intersection))
     vect1 vect2
     :on-first-missing increase-union
     :on-second-missing increase-union)
    (- 1.0 (/ intersection union))))


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


(defun add-data-point (outputs context data-point)
  (let* ((buffer (read-buffer outputs))
         (neurons (cl-htm.training:active-neurons context))
         (copy (make-array (length neurons) :element-type 'fixnum))
         (new-entry (cons copy data-point)))
    (map-into copy #'identity neurons)
    (vector-push-extend new-entry buffer))
  outputs)


(defun prediction (context output)
  cl-ds.utils:todo)

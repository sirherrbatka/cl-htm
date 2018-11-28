(in-package #:cl-htm.model)

(define-constant +offset-basis+ 2166136261)
(define-constant +prime+ 16777619)

(defun hash-vector (vector)
  (declare (type (simple-array fixnum (*)) vector)
           (optimize (speed 3)))
  (iterate
    (declare (type fixnum hash number))
    (with hash = +offset-basis+)
    (for number in-vector vector)
    (setf hash (logxor (ldb (byte 32 0) (* +prime+ hash))
                       number))
    (finally (return hash))))


(cl-custom-hash-table:define-custom-hash-table-constructor
    make-vector-hashtable
  :test vector=
  :hash-function hash-vector)


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
    :accessor access-stored-outputs
    :initform nil
    :documentation "Metric dictionary, mapping outputs to values.")
   (%test
    :initarg :test
    :reader read-test
    :documentation "Test for hash table.")
   (%buffer
    :initarg :buffer
    :initform (make-vector-hashtable)
    :accessor access-buffer
    :documentation "Temporary buffer for all outputs, before creating metric dictionary.")
   (%close-limit
    :initarg :close-limit
    :accessor access-close-limit
    :documentation "Limit for metric to trim search in the metric dictionary.")))


(defun clear-buffer (outputs)
  (setf (access-buffer outputs) (vect))
  outputs)


(defun make-counting-dictionary (outputs)
  (lret ((result (make-vector-hashtable)))
    (iterate
      (with test = (read-test outputs))
      (for (neurons . data) in-vector (access-buffer outputs))
      (for inner-hashtable = )
      (incf (gethash inner-hashtable data))
      (setf (gethash result neurons) inner-hashtable))))


(defun make-metric-dictionary (outputs)
  (let* ((counting-dictionary (read-buffer outputs))
         (size (hash-table-count counting-dictionary))
         (vector (make-array size)))
    (iterate
      (for i from 0)
      (for (neurons data) in-hashtable counting-dictionary)
      (setf (aref vector i) (cons neurons (hash-table-alist data))))
    (cl-ds:make-from-traversable 'cl-ds.ms.egnat:mutable-egnat-metric-set
                                 vector
                                 (compose #'vector= #'car)
                                 (fork #'jaccard-metric #'car #'car))))


(defun fill-dictionary (outputs)
  (setf (access-metric-dictionary) (make-metric-dictionary outputs))
  outputs)


(defun add-data-point (outputs context data-point)
  (let* ((buffer (access-buffer outputs))
         (neurons (cl-htm.training:active-neurons context))
         (copy (map-into (make-array (length neurons) :element-type 'fixnum)
                         #'identity
                         neurons))
         (test (read-test outputs))
         (inner (gethash copy buffer
                         (make-hash-table :test test))))
    (incf (gethash inner data-point))
    (setf (gethash copy buffer) inner))
  outputs)


(defun prediction (context output)
  cl-ds.utils:todo)

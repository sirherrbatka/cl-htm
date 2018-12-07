(in-package #:cl-htm.model)

(define-constant +offset-basis+ 2166136261)
(define-constant +prime+ 16777619)

(defun hash-vector (vector)
  (declare (type (simple-array fixnum (*)) vector)
           (optimize (speed 3)))
  (reduce (lambda (hash number)
            (declare (fixnum hash number))
            (logxor (ldb (byte 32 0) (* +prime+ hash))
                    number))
          vector
          :initial-value +offset-basis+))


(cl-custom-hash-table:define-custom-hash-table-constructor
    make-vector-hashtable
  :test vector=
  :hash-function hash-vector)


(defun jaccard-metric (vect1 vect2)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0))
           (type (array fixnum (*)) vect1 vect2))
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
  ((%mutex :initform (bt:make-lock)
           :reader read-mutex)
   (%stored-outputs
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


(defstruct decoder-output
  class count)


(defun make-metric-dictionary (outputs)
  (let* ((counting-dictionary (access-buffer outputs))
         (size (hash-table-count counting-dictionary))
         (vector (make-array size)))
    (iterate
      (for i from 0)
      (for (neurons data) in-hashtable counting-dictionary)
      (setf (aref vector i) (cons neurons
                                  (mapcar (lambda (x)
                                            (make-decoder-output :class (car x)
                                                                 :count (cdr x)))
                                          (hash-table-alist data)))))
    (cl-ds:make-from-traversable 'cl-ds.ms.egnat:mutable-egnat-metric-set
                                 vector
                                 (fork #'vector= #'car #'car)
                                 (lambda (a b) (jaccard-metric (car a) (car b)))
                                 ;; (fork #'jaccard-metric #'car #'car)
                                 'single-float)))


(defun fill-dictionary (outputs)
  (setf (access-stored-outputs outputs) (make-metric-dictionary outputs))
  outputs)


(defun add-data-point (outputs context data-point)
  (bt:with-lock-held ((read-mutex outputs))
    (let* ((buffer (access-buffer outputs))
           (neurons (cl-htm.training:active-neurons context))
           (copy (map-into (make-array (length neurons) :element-type 'fixnum)
                           #'first-elt
                           neurons))
           (test (read-test outputs))
           (inner (gethash copy buffer
                           (make-hash-table :test test))))
      (incf (gethash data-point inner 0))
      (setf (gethash copy buffer) inner))
    outputs))


(defun transform-vector (function vector)
  (map-into vector function vector))


(defun prediction (context output)
  (let ((outputs (access-stored-outputs output))
        (predictive-neurons (~> context
                                cl-htm.training:past-predictive-neurons
                                copy-array)))
    (when (null outputs)
      (fill-dictionary output)
      (setf outputs (access-stored-outputs output)))
    (~>> (access-close-limit output)
         (cl-ds:near
          outputs
          (list predictive-neurons))
         (cl-ds.alg:on-each
          (lambda (x)
            (let* ((neurons (first x))
                   (distance (jaccard-metric
                              predictive-neurons
                              neurons))
                   (data (rest x)))
              (mapcar (compose #'box (curry #'list* distance))
                      data))))
         cl-ds.alg:flatten-lists
         cl-ds.alg:to-vector
         (sort _ #'< :key (compose #'first-elt #'unbox))
         (transform-vector (compose #'cdr #'unbox)))))

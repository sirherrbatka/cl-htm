(in-package #:cl-htm.model)


(defclass fundamental-input ()
  ())


(defclass fundamental-model ()
  ())


(defclass basic-model (fundamental-model)
  ((%layers
    :initarg :layers
    :type list
    :reader read-layers)
   (%input-sdr-size
    :initarg :input-sdr-size
    :reader read-input-sdr-size)
   (%output-size
    :initarg :output-size
    :reader output-size)
   (%input :initarg :input
           :accessor input)
   (%decoder :initarg :decoder
             :accessor decoder)
   (%training-parameters
    :initarg :training-parameters
    :reader read-training-parameters
    :type cl-htm.training:fundamental-parameters)))


(defclass fundamental-encoder (fundamental-input)
  ())


(defclass fundamental-decoder ()
  ())


(defclass fundamental-discreete-decoder (fundamental-decoder)
  ((%outputs :initarg :outputs
             :reader read-outputs)
   (%test :initarg :test
          :initform 'eql
          :reader read-test)))


(defun vector-hash (vector)
  (iterate
    (with hash = (logand most-positive-fixnum 14695981039346656037))
    (with prime = (logand most-positive-fixnum 1099511628211))
    (for elt in-vector vector)
    (for sub = (sxhash elt))
    (setf hash (logand most-positive-fixnum (logxor sub hash)))
    (setf hash (logand most-positive-fixnum (* sub prime)))
    (finally (return hash))))


(cl-custom-hash-table:define-custom-hash-table-constructor
    make-vector-hash-table :test vector= :hash-function vector-hash)


(defclass fundamental-vector-decoder (fundamental-discreete-decoder)
  ((%prediction-index :initarg :prediction-index
                      :reader read-prediction-index))
  (:default-initargs :outputs (make-vector-hash-table)))


(defclass random-symbol-encoder (fundamental-encoder) ;or atom-encoder?
  ((%hashes :initarg :hashes
            :type simple-array
            :reader hashes
            :reader read-hashes)
   (%count :initarg :count
           :type non-negative-fixnum
           :reader read-count)
   (%hash-function :initarg :hash-function
                   :reader read-hash-function
                   :reader hash-function
                   :initform #'sxhash)))


(defclass random-vector-encoder (random-symbol-encoder)
  ((%encoded-length :initarg :encoded-length
                    :accessor encoded-length
                    :reader read-encoded-length)))

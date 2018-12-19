(in-package #:cl-htm.utils)


(defclass lazy-vector ()
  ((%inner-vector :type simple-vector
                  :initarg :inner-vector
                  :reader read-inner-vector)))


(defclass potential-lazy-vector (lazy-vector)
  ((%data-range :type cl-ds:fundamental-range
                :reader read-data-range)))


(defgeneric at (vector position))


(defmethod at ((vector lazy-vector) position)
  (aref (the simple-vector (slot-value vector '%inner-vector)) position))


(defmethod at ((vector potential-lazy-vector) position)
  (let* ((inner-vector (slot-value vector '%inner-vector))
         (length (array-dimension inner-vector 0)))
    (declare (type simple-vector inner-vector)
             (type fixnum length))
    (unless (< position length)
      (let ((new-vector (make-array
                         (+ position 1)
                         :element-type (array-element-type inner-vector)))
            (range (read-data-range vector)))
        (map-into new-vector #'identity inner-vector)
        (iterate
          (with range = (read-data-range vector))
          (for i from length to position)
          (for (values v m) = (cl-ds:consume-front range))
          (unless m
            (error "Not enough data to fill the vector!"))
          (setf (aref new-vector i) v))
        (unless (~>> range cl-ds:peek-front (nth-value 1))
          (change-class vector 'lazy-vector))
        (setf (slot-value vector '%inner-vector) new-vector)))
    (call-next-method)))

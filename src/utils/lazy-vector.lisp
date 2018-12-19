(in-package #:cl-htm.utils)


(defclass lazy-vector ()
  ((%inner-vector :type simple-vector
                  :initarg :inner-vector
                  :reader read-inner-vector)))


(defclass potential-lazy-vector (lazy-vector)
  ((%data-range :type cl-ds:fundamental-range
                :reader read-data-range)))


(defgeneric at (vector position))


(defgeneric (setf at) (vector position))


(defmethod at ((vector lazy-vector) position)
  (aref (the simple-vector (slot-value vector '%inner-vector)) position))


(defmethod at ((vector potential-lazy-vector) position)
  (let* ((vector (slot-value vector '%inner-vector))
         (length (array-dimension vector 0)))
    (declare (type simple-vector vector)
             (type fixnum length))
    (unless (< position length)
      (iterate
        (with range = (read-data-range vector))
        (for i from length to position)
        (for (values v m) = (cl-ds:consume-front range))
        (unless m
          (error "Not enough data to fill the vector!"))
        (setf (aref vector i) v)))
    (call-next-method)))

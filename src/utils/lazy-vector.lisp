(in-package #:cl-htm.utils)


(defclass lazy-vector ()
  ((%inner-vector :type simple-vector
                  :initarg :inner-vector
                  :reader read-inner-vector)))


(defclass potential-lazy-vector (lazy-vector)
  ((%data-range :type cl-ds:fundamental-range
                :initarg :data-range
                :reader read-data-range)))


(defgeneric at (vector position))


(defgeneric matching (vector test-fn &key key))


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


(defmethod matching ((vector lazy-vector) test-fn &key (key #'identity))
  (let* ((inner-vector (slot-value vector '%inner-vector))
         (index (position-if test-fn inner-vector :key key)))
    (declare (type simple-vector inner-vector))
    (unless (null index)
      (aref inner-vector index))))


(defmethod matching ((vector potential-lazy-vector) test-fn &key (key #'identity))
  (let* ((inner-vector (slot-value vector '%inner-vector))
         (length (array-dimension inner-vector 0)))
    (iterate
      (declare (type fixnum i))
      (for i from 0 below length)
      (for value = (aref inner-vector i))
      (when (funcall test-fn (funcall key value))
        (return-from matching value)))
    (iterate
      (with range = (read-data-range vector))
      (with found? = nil)
      (with collected = nil)
      (for i from 0)
      (for (values value more) = (cl-ds:consume-front range))
      (while more)
      (push value collected)
      (when (funcall test-fn (funcall key value))
        (setf found? t)
        (finish))
      (finally
       (unless (~>> range cl-ds:peek-front (nth-value 1))
         (change-class vector 'lazy-vector))
       (return
         (unless (zerop i)
           (let* ((new-size (+ length i))
                  (new-vector (make-array
                               new-size
                               :element-type (array-element-type inner-vector))))
             (iterate
               (for j from new-size downto length)
               (for c in collected)
               (setf (aref new-vector j) c))
             (setf (slot-value vector '%inner-vector) new-vector)
             (when found?
               (first collected))))))))))


(defun make-lazy-vector (element-type range)
  (make-instance 'potential-lazy-vector
                 :inner-vector (make-array 0 :element-type element-type)
                 :data-range range))

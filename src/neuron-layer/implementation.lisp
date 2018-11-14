(in-package #:cl-htm.nl)


(define-constant +empty-vector+ (make-array 0 :element-type 'fixnum)
  :test 'vector=)


(defmethod calculate-active-synapses-for-columns ((layer neuron-layer)
                                                  (columns neuron-column)
                                                  (input cl-htm.sdr:sdr))
  (nest
   (vector-classes:with-data (((column-input input))
                              columns i neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input j cl-htm.sdr:sdr))
   (iterate
     (with size = (vector-classes:size columns))
     (with syn-count = (array-dimension column-input 1))
     (with result = (make-array size :element-type 'fixnum))
     (for i from 0 below size)
     (setf (aref result i)
           (iterate
             (for k from 0 below syn-count)
             (for j = (column-input k))
             (sum (active))))
     (finally (return result)))))


(defmethod select-active-columns ((layer neuron-layer)
                                  (columns neuron-column)
                                  active-synapses)
  (check-type active-synapses (simple-array fixnum (*)))
  (~> (read-column-indices layer)
      (cl-ds.utils:select-top (read-activated-columns-count layer)
                              #'> :key
                              (curry #'aref active-synapses))
      (sort #'<)))


(defmethod select-predictive-neurons ((layer neuron-layer)
                                      (sdr cl-htm.sdr:sdr)
                                      (columns neuron-column)
                                      active-columns)
  (check-type active-columns simple-vector)
  (nest
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron-index neuron-layer))
   (vector-classes:with-data (((columns-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              sdr input-index cl-htm.sdr:sdr))
   (let ((threshold (read-threshold layer))
         (column-size (/ (vector-classes:size layer)
                         (vector-classes:size columns)))
         (result (make-array
                  0 :adjustable t
                    :fill-pointer 0
                    :element-type 'non-negative-fixnum))
         (synapses-count (array-dimension synapses-strength 1)))
     (iterate
       (for i from 0)
       (for column-index in-vector active-columns)
       (for column-start = (* column-index column-size))
       (iterate
         (for neuron-index from column-start)
         (repeat column-size)
         (for value = (iterate
                        (for k from 0 below synapses-count)
                        (for input-index = (columns-input k))
                        (when (active)
                          (multiplying (synapses-strength k)))))
         (when (> value threshold)
           (vector-push-extend neuron-index result))
         (finally (return result)))))))


(defun selecting-the-most-active-neuron (layer columns input)
  (nest
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron-index neuron-layer))
   (vector-classes:with-data (((columns-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input input-index cl-htm.sdr:sdr))
   (let ((column-size (/ (vector-classes:size layer)
                         (vector-classes:size columns)))
         (synapses-count (array-dimension synapses-strength 1))))
   (lambda (column-index))
   (iterate
     (with column-start = (* column-index column-size))
     (with result = 0)
     (for neuron-index from column-start)
     (repeat column-size)
     (for value = (iterate
                    (for k from 0 below synapses-count)
                    (for input-index = (columns-input k))
                    (when (active)
                      (multiplying (synapses-strength k)))))
     (maximize value into maxi)
     (when (= maxi value)
       (setf result neuron-index))
     (finally (return result)))))


(defmethod select-active-neurons ((layer neuron-layer)
                                  (columns neuron-column)
                                  (input cl-htm.sdr:sdr)
                                  active-columns
                                  predictive-neurons)
  (let ((column-size (/ (vector-classes:size layer)
                        (vector-classes:size columns)))
        (active-neurons (make-array
                         0 :element-type 'non-negative-fixnum
                           :adjustable t)))
    (cl-ds.utils:on-ordered-intersection
     (lambda (column neuron) (declare (ignore column))
       (vector-push-extend neuron active-neurons))
     active-columns
     predictive-neurons
     :same #'eql
     :on-first-missing (compose (rcurry #'vector-push-extend active-neurons)
                                (selecting-the-most-active-neuron layer
                                                                  columns
                                                                  input))
     :second-key (lambda (neuron) (floor neuron column-size)))
    active-neurons))


(defmethod activate ((layer neuron-layer)
                     (sdr cl-htm.sdr:sdr)
                     context
                     &optional (prev-data +empty-vector+))
  ;; calculate number of active synapses for each column
  ;; select top active columns
  ;; select predictive neurons
  ;; set active neurons
  ;; finally, return all predictive neurons
  (let* ((columns (read-columns layer))
         (active-synapses-for-columns (calculate-active-synapses-for-columns
                                       layer columns sdr))
         (active-columns (select-active-columns columns
                                                active-synapses-for-columns))
         (predictive-neurons (select-predictive-neurons layer
                                                        columns
                                                        active-columns))
         (active-neurons (select-active-neurons layer sdr columns
                                                active-columns prev-data)))
    (update-synapses layer columns active-columns prev-data active-neurons)
    predictive-neurons))

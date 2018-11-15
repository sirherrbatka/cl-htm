(in-package #:cl-htm.nl)


(define-constant +empty-vector+ (make-array 0 :element-type 'fixnum)
  :test 'vector=)


(defmethod calculate-active-synapses-for-columns
    ((layer neuron-layer)
     (input cl-htm.sdr:sdr)
     (columns neuron-column))
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
                                  (training-parameters fundamental-training-parameters)
                                  (columns neuron-column)
                                  active-synapses)
  (check-type active-synapses (simple-array fixnum (*)))
  (~> (read-column-indices layer)
      (cl-ds.utils:select-top (activated-columns-count training-parameters)
                              #'> :key
                              (curry #'aref active-synapses))
      (sort #'<))) ; could be some bucket sort to speed things up (but probabbly won't change that much)


(defmethod select-predictive-neurons ((layer neuron-layer)
                                      (sdr cl-htm.sdr:sdr)
                                      (training-parameters fundamental-training-parameters)
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
   (let ((threshold (threshold training-parameters))
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
   (let ((column-size (truncate (vector-classes:size layer)
                                (vector-classes:size columns)))
         (synapses-count (array-dimension synapses-strength 1)))
     (declare (type non-negative-fixnum column-size synapses-count)))
   (lambda (column-index))
   (iterate
     (declare (type non-negative-fixnum column-start result
                    neuron-index)
              (type single-float maxi value))
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
  (check-type predictive-neurons (vector non-negative-fixnum))
  (check-type active-columns (vector non-negative-fixnum))
  (let ((column-size (truncate (vector-classes:size layer)
                               (vector-classes:size columns)))
        (active-neurons (make-array
                         0 :element-type 'non-negative-fixnum
                           :adjustable t)))
    (declare (type non-negative-fixnum column-size))
    (cl-ds.utils:on-ordered-intersection
     (lambda (column neuron)
       (declare (ignore column)
                (type non-negative-fixnum neuron))
       (vector-push-extend neuron active-neurons))
     active-columns
     predictive-neurons
     :same #'eql
     :on-first-missing (compose (rcurry #'vector-push-extend active-neurons)
                                (selecting-the-most-active-neuron layer
                                                                  columns
                                                                  input))
     :second-key (lambda (neuron) (truncate neuron column-size)))
    active-neurons))


(defmethod update-synapses ((layer neuron-layer)
                            (input cl-htm.sdr:sdr)
                            (training-parameters fundamental-training-parameters)
                            (columns neuron-column)
                            active-columns
                            predictive-neurons
                            active-neurons)
  (check-type predictive-neurons (vector non-negative-fixnum))
  (check-type active-columns (vector non-negative-fixnum))
  (check-type active-neurons (vector non-negative-fixnum))
  (nest
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input input-index cl-htm.sdr:sdr))
   (vector-classes:with-data (((column-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron neuron-layer))
   (let ((decay (decay layer))
         (synapses-count (array-dimension synapses-strength 1))
         (column-size (truncate (the fixnum (vector-classes:size layer))
                                (the fixnum (vector-classes:size columns))))
         (p+ (p+ layer))
         (p- (p- layer)))
     (declare (type single-float decay p+ p-)
              (type non-negative-fixnum column-size synapses-count))
     (cl-ds.utils:on-ordered-intersection
      (lambda (active-neuron predictive-neuron)
        (declare (ignore predictive-neuron)
                 (type non-negative-fixnum active-neuron))
        ;; reinforce some of the neurons...
        (let* ((neuron active-neuron)
               (column-index (truncate neuron synapses-count)))
          (declare (type non-negative-fixnum column-index neuron))
          (iterate
            (for i from 0 below synapses-count)
            (for input-index = (column-input i))
            (unless (zerop (active))
              (incf (synapses-strength i) p+)))))
      active-neurons
      predictive-neurons
      :same #'eql
      :on-first-missing (lambda (x) ; punish other neurons...
                          cl-ds.utils:todo))
     ;; decay everything
     cl-ds.utils:todo)))


(defmethod activate ((layer neuron-layer)
                     (sdr cl-htm.sdr:sdr)
                     (training-parameters fundamental-training-parameters)
                     &optional (prev-data +empty-vector+))
  ;; calculate number of active synapses for each column
  ;; select top active columns
  ;; select predictive neurons
  ;; set active neurons
  ;; finally, return all predictive neurons
  (let* ((columns (columns layer))
         (active-synapses-for-columns (calculate-active-synapses-for-columns
                                       layer sdr columns))
         (active-columns (select-active-columns layer
                                                training-parameters
                                                columns
                                                active-synapses-for-columns))
         (predictive-neurons (select-predictive-neurons layer
                                                        sdr
                                                        training-parameters
                                                        columns
                                                        active-columns))
         (active-neurons (select-active-neurons layer sdr columns
                                                active-columns prev-data)))
    (update-synapses layer sdr training-parameters columns
                     active-columns prev-data active-neurons)
    predictive-neurons))

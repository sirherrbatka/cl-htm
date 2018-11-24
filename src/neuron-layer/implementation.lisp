(in-package #:cl-htm.nl)


(define-constant +empty-vector+ (make-array 0 :element-type 'non-negative-fixnum)
  :test 'vector=)
(declaim (type (vector non-negative-fixnum) +empty-vector+))


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


(defmethod select-active-columns
    ((layer neuron-layer)
     (training-parameters cl-htm.training:fundamental-parameters)
     (columns neuron-column)
     active-synapses)
  (check-type active-synapses (simple-array fixnum (*)))
  (let* ((activated-columns-fraction (cl-htm.training:activated-columns-fraction
                                      training-parameters))
         (activated-columns-count (~> (vector-classes:size columns)
                                      (* activated-columns-fraction)
                                      floor)))
    (~> (read-column-indices columns)
        (cl-ds.utils:select-top activated-columns-count
                                #'> :key
                                (curry #'aref active-synapses))
        (sort #'<)))) ; could be some bucket sort to speed things up (but probabbly won't change that much)


(defmethod select-predictive-neurons
    ((layer neuron-layer)
     (sdr cl-htm.sdr:sdr)
     (training-parameters cl-htm.training:fundamental-parameters)
     (columns neuron-column)
     active-columns)
  (check-type active-columns vector)
  (nest
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron-index neuron-layer))
   (vector-classes:with-data (((columns-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              sdr input-index cl-htm.sdr:sdr))
   (let ((threshold (cl-htm.training:threshold training-parameters))
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
                          (summing (synapses-strength k)))))
         (when (> value threshold)
           (vector-push-extend neuron-index result))))
     result)))


(defun selecting-the-most-active-neuron (layer columns input)
  (declare (optimize (debug 3)))
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
     (declare (type fixnum column-start result neuron-index)
              (type single-float maxi value))
     (with column-start = (* column-index column-size))
     (with result = 0)
     (for neuron-index from column-start)
     (repeat column-size)
     (for value = (iterate
                    (for k from 0 below synapses-count)
                    (for input-index = (columns-input k))
                    (when (active)
                      (summing (synapses-strength k)))))
     (maximize value into maxi)
     (when (= maxi value)
       (setf result neuron-index))
     (finally (return result)))))


(defmethod select-active-neurons ((layer neuron-layer)
                                  (columns neuron-column)
                                  (input cl-htm.sdr:sdr)
                                  active-columns
                                  predictive-neurons
                                  active-neurons)
  (check-type predictive-neurons (vector non-negative-fixnum))
  (check-type active-columns vector)
  (let ((column-size (truncate (vector-classes:size layer)
                               (vector-classes:size columns))))
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


(defmethod update-synapses
    ((training-parameters cl-htm.training:fundamental-parameters)
     (layer neuron-layer)
     (input cl-htm.sdr:sdr)
     (mode cl-htm.training:train-mode)
     (columns neuron-column)
     active-columns
     predictive-neurons
     active-neurons)
  (declare (optimize (debug 3)))
  (check-type predictive-neurons (vector non-negative-fixnum))
  (check-type active-columns vector)
  (check-type active-neurons (vector non-negative-fixnum))
  (nest
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input input-index cl-htm.sdr:sdr))
   (vector-classes:with-data (((column-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron neuron-layer))
   (let* ((decay (cl-htm.training:decay training-parameters))
          (p+ (cl-htm.training:p+ training-parameters))
          (p- (cl-htm.training:p- training-parameters))
          (synapses-count (array-dimension synapses-strength 1))
          (column-count (vector-classes:size columns))
          (column-size (truncate (the fixnum (vector-classes:size layer))
                                 column-count)))
     (declare (type single-float decay p+ p-)
              (type non-negative-fixnum column-size synapses-count
                    column-count)))
   (flet ((change-synapses
              (neuron &aux (column-index (truncate neuron column-size)))
            (declare (type non-negative-fixnum column-index neuron))
            (iterate
              (for i from 0 below synapses-count)
              (for input-index = (column-input i))
              (if (zerop (active))
                  (setf (synapses-strength i)
                        (~> (synapses-strength i)
                            (- p-)
                            (max single-float-epsilon)))
                  (incf (synapses-strength i) p+))))))
   (cl-ds.utils:on-ordered-intersection
    (lambda (active-neuron predictive-neuron)
      (declare (ignore predictive-neuron))
      (change-synapses active-neuron))
    active-neurons
    predictive-neurons
    :same #'eql
    :on-first-missing (lambda (neuron)
                        (iterate
                          (with column-index =
                                (truncate neuron column-size))
                          (for i from 0 below synapses-count)
                          (for input-index = (column-input i))
                          (unless (zerop (active))
                            (setf (synapses-strength i)
                                  (~> (synapses-strength i)
                                      (- decay)
                                      (max single-float-epsilon))))))
    :on-second-missing #'change-synapses))
  nil)


(defmethod to-sdr ((neuron neuron-layer-weights))
  (lret ((result (make 'neuron-layer)))
    (setf (slot-value result 'vector-classes::%size)
          (vector-classes:size neuron)

          (slot-value result 'cl-htm.sdr:active-neurons)
          (make-array (vector-classes:size neuron)
                      :element-type 'bit)

          (slot-value result '%columns) (columns neuron)

          (slot-value result 'synapses-strength)
          (slot-value neuron 'synapses-strength))))


(defmethod update-synapses
    ((training-parameters cl-htm.training:fundamental-parameters)
     (layer neuron-layer)
     (input cl-htm.sdr:sdr)
     (mode cl-htm.training:predict-mode)
     (columns neuron-column)
     active-columns
     predictive-neurons
     active-neurons)
  nil)


(defmethod activate
    ((layer neuron-layer)
     (sdr cl-htm.sdr:sdr)
     (context cl-htm.training:fundamental-context)
     (training-parameters cl-htm.training:fundamental-parameters)
     (mode cl-htm.training:fundamental-mode))
  ;; calculate number of active synapses for each column
  ;; select top active columns
  ;; select predictive neurons
  ;; set active neurons
  ;; finally, return all predictive neurons
  (let* ((columns (columns layer))
         (prev-data (cl-htm.training:past-predictive-neurons context))
         (active-neurons (cl-htm.training:active-neurons context))
         (active-synapses-for-columns
           (calculate-active-synapses-for-columns
            layer sdr columns))
         (active-columns (select-active-columns
                          layer
                          training-parameters
                          columns
                          active-synapses-for-columns))
         (predictive-neurons (select-predictive-neurons
                              layer
                              sdr
                              training-parameters
                              columns
                              active-columns)))
    (setf (fill-pointer active-neurons) 0

          (cl-htm.training:past-predictive-neurons context)
          predictive-neurons)
    (select-active-neurons layer columns sdr
                           active-columns prev-data
                           active-neurons)
    (vector-classes:with-data (((neuron cl-htm.sdr:active-neurons))
                               layer
                               i
                               neuron-layer)
      (map nil (lambda (i) (setf (neuron) 1))
           active-neurons))
    (update-synapses training-parameters layer sdr mode columns
                     active-columns prev-data active-neurons)
    layer))


(defmethod context ((layer neuron-layer))
  (make 'cl-htm.training:basic-context))


(defmethod layer :before ((type symbol)
                          &key size column-count synapses-count input-size)
  (check-type size positive-integer)
  (check-type input-size (or null positive-integer))
  (check-type synapses-count positive-integer)
  (check-type column-count positive-integer))


(defmethod layer (type &rest args)
  (make 'layer :arguments args
               :type type))


(defmethod make-weights ((type (eql 'neuron-layer-weights))
                         input-size
                         &key size column-count synapses-count)
  (check-type column-count positive-fixnum)
  (check-type synapses-count positive-fixnum)
  (check-type size positive-fixnum)
  (let ((column-size (/ size column-count)))
    (check-type column-size positive-integer))
  (lret ((result (vector-classes:make-data
                  'neuron-layer-weights
                  size
                  :synapses-strength (list synapses-count)
                  :columns (vector-classes:make-data
                            'neuron-column
                            column-count
                            :input-size (list synapses-count)
                            :column-indices (coerce (iota column-count)
                                                    '(vector fixnum))))))
    (vector-classes:with-data (((input input))
                               (columns result)
                               i
                               neuron-column)
      (iterate
        (with indices = (~> input-size iota (coerce '(vector fixnum))))
        (for i from 0 below (vector-classes:size (columns result)))

        (iterate
          (for j from 0 below synapses-count)
          (for index in-vector (shuffle indices))
          (setf (input j) index))))))


(defmethod effective-layer ((layer layer)
                            (prev integer))
  (apply #'make-weights
         (read-type layer)
         prev
         (read-arguments layer)))


(defmethod effective-layer ((layer layer)
                            (prev neuron-layer-weights))
  (apply #'make-weights
         (read-type layer)
         (vector-classes:size prev)
         (read-arguments layer)))


(defmethod effective-layers ((declared-layers list)
                             initial-size)
  (iterate
    (for layer in declared-layers)
    (for prev-layer previous layer initially initial-size)
    (collect (effective-layer layer initial-size))))

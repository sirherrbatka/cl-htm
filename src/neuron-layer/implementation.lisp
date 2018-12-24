(in-package #:cl-htm.nl)


(define-constant +empty-vector+ (make-array 0 :element-type 'fixnum)
  :test 'vector=)
(declaim (type (vector fixnum) +empty-vector+))


(defmethod calculate-active-synapses-for-columns
    ((layer neuron-layer)
     (input cl-htm.sdr:sdr)
     (columns neuron-column))
  (declare (optimize (speed 3) (safety 0)
                     (space 0) (debug 0)))
  (nest
   (vector-classes:with-data (((column-input input))
                              columns i neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input j cl-htm.sdr:sdr))
   (let* ((size (vector-classes:size columns))
          (segment-count (array-dimension column-input 1))
          (synapses-count (array-dimension column-input 2))
          (result (make-array size :element-type 'non-negative-fixnum)))
     (declare (type fixnum synapses-count size))
     (iterate
       (declare (type fixnum i))
       (for i from 0 below size)
       (setf (aref result i) i))
     (map-into
      result
      (lambda (i)
        (declare (type fixnum i))
        (iterate
          (declare (type fixnum s)
                   (type non-negative-fixnum sum))
          (with sum = 0)
          (for s from 0 below segment-count)
          (iterate
            (declare (type fixnum syn j))
            (for syn from 0 below synapses-count)
            (for j = (column-input s syn))
            (incf sum (active)))
          (finally (return sum))))
      result))))


(defstruct segment
  (source (make-array 0 :element-type 'fixnum) :type (array fixnum (*)))
  (weight (make-array 0 :element-type 'fixnum) :type (array fixnum (*))))


(defun build-segment (layer synapses-count)
  (let ((weights (make-array synapses-count :element-type 'fixnum))
        (layer-size (vector-classes:size layer))
        (source (make-array synapses-count :element-type 'fixnum)))
    (map-into weights #'cl-htm.utils:random-synapses-strength)
    (iterate
      (for i from 0 below synapses-count)
      (setf (aref source i) (random layer-size)))
    (make-segement :source source :weight weight)))


(defun distal-segment (layer neuron-index)
  (let ((segment-count (access-segments-count layer))
        (synapses-count (access-synapses-count layer)))
    (vector-classes:with-data (((segment distal-segments))
                               layer
                               neuron-index
                               neuron-layer)
      (ensure (segment)
        (cl-htm.utils:make-lazy-vector
         t
         (cl-ds:xpr (:i 0)
           (let ((new-i (1+ i)))
             (if (< new-i segment-count)
                 (cl-ds:send-recur
                  (build-segment layer synapses-count)
                  :i new-i)
                 (cl-ds:send-finish
                  (build-segment layer synapses-count))))))))))


(defmethod select-active-columns
    ((layer neuron-layer)
     (training-parameters cl-htm.training:fundamental-parameters)
     (columns neuron-column)
     active-synapses)
  (check-type active-synapses (simple-array non-negative-fixnum (*)))
  (let* ((activated-columns-fraction (cl-htm.training:activated-columns-fraction
                                      training-parameters))
         (activated-columns-count (~> (vector-classes:size columns)
                                      (* activated-columns-fraction)
                                      floor)))
    (~> (read-column-indices columns)
        copy-array
        (cl-ds.utils:select-top activated-columns-count
                                #'> :key
                                (lambda (x) (aref active-synapses x)))
        (sort #'<)))) ; could be some bucket sort to speed things up (but probabbly won't change that much)


(defmethod select-predictive-neurons
    ((layer neuron-layer)
     (sdr cl-htm.sdr:sdr)
     (training-parameters cl-htm.training:fundamental-parameters)
     (columns neuron-column)
     active-columns)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (check-type active-columns (array fixnum (*)))
  (nest
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron-index neuron-layer))
   (vector-classes:with-data (((columns-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              sdr input-index cl-htm.sdr:sdr))
   (let* ((threshold (cl-htm.training:threshold training-parameters))
          (column-size (/ (the fixnum (vector-classes:size layer))
                          (the fixnum (vector-classes:size columns))))
          (result (make-array
                   (truncate column-size 10)
                   :adjustable t
                   :fill-pointer 0
                   :element-type t))
          (segments-count (array-dimension columns-input 1))
          (synapses-count (array-dimension columns-input 2))
          (segment-pointers (make-array segments-count
                                        :element-type 'fixnum
                                        :initial-element 0))
          (synapses-array (make-array (list segments-count synapses-count)
                                      :element-type 'fixnum)))
     (declare (type fixnum threshold column-size
                    synapses-count segments-count)
              (type vector result))
     (map
      nil
      (lambda (column-index)
        (declare (type fixnum column-index))
        (iterate
          (for k from 0 below segments-count)
          (iterate
            (declare (type fixnum k input-index))
            (for h from 0 below synapses-count)
            (for input-index = (columns-input k h))
            (unless (zerop (active))
              (setf (aref synapses-array k (aref segment-pointers k))
                    h)
              (incf (aref segment-pointers k)))))
        (iterate
          (declare (type fixnum i neuron-index column-start))
          (with column-start = (* column-index column-size))
          (for neuron-index from column-start)
          (for i from 0 below column-size)
          (vector-push-extend nil result)
          (iterate outer
            (for s from 0 below segments-count)
            (iterate
              (declare (type fixnum sum k i))
              (with sum = 0)
              (for i from 0 below (aref segment-pointers s))
              (for k = (aref synapses-array s i))
              (incf sum (synapses-strength s k))
              (when (> sum threshold)
                (let ((target
                        (ensure (last-elt result)
                          (make-array 16 :element-type 'fixnum
                                         :adjustable t
                                         :fill-pointer 1
                                         :initial-element neuron-index))))
                  (vector-push-extend s target))
                (leave))))
          (when (~> result last-elt null)
            (decf (fill-pointer result)))))
      active-columns)
     result)))


(defun selecting-the-most-active-neuron (layer columns input)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (nest
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron-index neuron-layer))
   (vector-classes:with-data (((columns-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input input-index cl-htm.sdr:sdr))
   (let* ((column-size (truncate (the fixnum (vector-classes:size layer))
                                (the fixnum (vector-classes:size columns))))
          (segments-count (array-dimension columns-input 1))
          (synapses-count (array-dimension columns-input 2))
          (segment-pointers (make-array segments-count
                                        :element-type 'fixnum
                                        :initial-element 0))
          (synapses-array (make-array (list segments-count synapses-count)
                                      :element-type 'fixnum)))
     (declare (type fixnum column-size synapses-count)))
   (lambda (column-index)
     (declare (type fixnum column-index))
     (iterate
       (for k from 0 below segments-count)
       (iterate
         (declare (type fixnum k input-index))
         (for h from 0 below synapses-count)
         (for input-index = (columns-input k h))
         (unless (zerop (active))
           (setf (aref synapses-array k (aref segment-pointers k))
                 h)
           (incf (aref segment-pointers k)))))
     (iterate
       (declare (type fixnum segment i
                      neuron-index column-start
                      sum))
       (with column-start = (* column-index column-size))
       (with result = (make-array 2 :element-type 'fixnum))
       (for neuron-index from column-start)
       (for i from 0 below column-size)
       (for sum = 0)
       (for segment = 0)
       (iterate outer
         (declare (type fixnum s segment-signal))
         (for s from 0 below segments-count)
         (for segment-signal =
              (iterate
                (declare (type fixnum k i))
                (for i from 0 below (aref segment-pointers s))
                (for k = (aref synapses-array s i))
                (sum (synapses-strength s k))))
         (incf sum segment-signal)
         (maximize segment-signal into maxi)
         (when (eql maxi segment-signal)
           (setf segment s)))
       (maximize sum into maxi)
       (when (= maxi sum)
         (setf (aref result 0) neuron-index
               (aref result 1) segment))
       (finally (return result))))))


(defmethod select-active-neurons ((layer neuron-layer)
                                  (columns neuron-column)
                                  (input cl-htm.sdr:sdr)
                                  active-columns
                                  predictive-neurons
                                  active-neurons)
  (check-type predictive-neurons vector)
  (check-type active-columns (simple-array fixnum (*)))
  (check-type active-neurons (array * (*)))
  (setf (fill-pointer active-neurons) 0)
  (let ((column-size (truncate (vector-classes:size layer)
                               (vector-classes:size columns))))
    (declare (type non-negative-fixnum column-size))
    (cl-ds.utils:on-ordered-intersection
     (lambda (column neuron)
       (declare (ignore column))
       (vector-push-extend neuron active-neurons))
     active-columns
     predictive-neurons
     :same #'eql
     :on-first-missing (compose (rcurry #'vector-push-extend active-neurons)
                                (selecting-the-most-active-neuron layer
                                                                  columns
                                                                  input))
     :second-key (lambda (neuron)
                   (truncate (first-elt neuron)
                             column-size)))
    active-neurons))


(defmethod update-synapses
    ((parameters cl-htm.training:fundamental-parameters)
     (layer neuron-layer)
     (input cl-htm.sdr:sdr)
     (mode cl-htm.training:train-mode)
     (columns neuron-column)
     active-columns
     predictive-neurons
     active-neurons)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (check-type predictive-neurons vector)
  (check-type active-columns (simple-array * (*)))
  (check-type active-neurons vector)
  (nest
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input input-index cl-htm.sdr:sdr))
   (vector-classes:with-data (((column-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer neuron neuron-layer))
   (let* ((decay (cl-htm.training:decay parameters))
          (p+ (cl-htm.training:p+ parameters))
          (p- (cl-htm.training:p- parameters))
          (synapses-count (array-dimension column-input 2))
          (column-count (vector-classes:size columns))
          (maximum-weight (cl-htm.training:maximum-weight parameters))
          (minimum-weight (cl-htm.training:minimum-weight parameters))
          (column-size (truncate (the fixnum (vector-classes:size layer))
                                 column-count)))
     (declare (type fixnum decay p+ p-
                    maximum-weight minimum-weight)
              (type non-negative-fixnum column-size
                    synapses-count column-count))
     (cl-ds.utils:on-ordered-intersection
      ;; reinforce!
      (lambda (active-neuron predictive-neuron)
        (declare (ignore predictive-neuron)
                 (type vector active-neuron))
        (iterate
          (declare (type non-negative-fixnum neuron column-index)
                   (type fixnum i segment))
          (with neuron = (first-elt active-neuron))
          (with column-index = (truncate neuron column-size))
          (for i from 1 below (length active-neuron))
          (for segment = (aref active-neuron i))
          (iterate
            (for synaps from 0 below synapses-count)
            (for input-index = (column-input segment synaps))
            (if (zerop (active))
                (setf (synapses-strength segment synaps)
                      (~> (synapses-strength segment synaps)
                          (- p-)
                          (max minimum-weight)))
                (setf (synapses-strength segment synaps)
                      (~> (synapses-strength segment synaps)
                          (+ p+)
                          (min maximum-weight)))))))
      active-neurons
      predictive-neurons
      :same #'eql
      :first-key #'first-elt
      :second-key #'first-elt
      ;; decaying active segments of inactive neurons
      :on-first-missing (lambda (vector)
                          (declare (type vector vector))
                          (iterate
                            (declare (type fixnum segment neuron length))
                            (with neuron = (first-elt vector))
                            (with length = (length vector))
                            (for segment-index
                                 from 1
                                 below length)
                            (for segment = (aref vector segment-index))
                            (iterate
                              (for synaps from 0 below synapses-count)
                              (setf (synapses-strength segment synaps)
                                    (~> (synapses-strength segment synaps)
                                        (- decay)
                                        (max minimum-weight)))))))
     :on-second-missing (lambda (vector)
                          (declare (type vector vector))
                          (iterate
                            (with neuron = (first-elt vector))
                            (with length = (length vector))
                            (for segment-index from 1 below length)
                            (for segment = (aref vector segment-index))
                            (iterate
                              (for synaps from 0 below synapses-count)
                              (setf (synapses-strength segment synaps)
                                    (~> (synapses-strength segment synaps)
                                        (+ p+)
                                        (min minimum-weight))))))))
  nil)


(defmethod to-effective-layer ((neuron neuron-layer-weights))
  (lret ((result (make 'neuron-layer)))
    (setf (slot-value result 'vector-classes::%size)
          (vector-classes:size neuron)

          (access-input-size neuron) (access-input-size neuron)
          (access-synapses-count result) (access-synapses-count result)
          (access-segments-count result) (access-segments-count result)

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
  (declare (optimize (speed 3) (safety 1)))
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
         (all-locks (read-locks columns))
         (active-columns (select-active-columns
                          layer
                          training-parameters
                          columns
                          active-synapses-for-columns))
         (locks (map 'vector
                     (lambda (i)
                       (lret ((lock (aref all-locks i)))
                         (bt:acquire-lock lock)))
                     active-columns)))
    (unwind-protect
         (let* ((predictive-neurons (select-predictive-neurons
                                     layer
                                     sdr
                                     training-parameters
                                     columns
                                     active-columns)))
           (declare (type (array fixnum (*))
                          active-columns)
                    (type (array * (*)) predictive-neurons)
                    (type (simple-array bt:lock (*)) all-locks locks))
           (select-active-neurons layer columns sdr
                                  active-columns prev-data
                                  active-neurons)
           (update-synapses training-parameters layer sdr mode columns
                            active-columns prev-data active-neurons)
           (setf (cl-htm.training:past-predictive-neurons context)
                 predictive-neurons)
           sdr)
      (map nil #'bt:release-lock locks))
    (vector-classes:with-data (((neuron cl-htm.sdr:active-neurons))
                               sdr
                               i
                               cl-htm.sdr:sdr)
      (cl-htm.sdr:clear-all-active sdr)
      (map nil (lambda (v &aux (i (first-elt v))) (setf (neuron) 1))
           active-neurons)
      (setf (cl-htm.sdr:dense-active-neurons sdr) active-neurons))))


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
                         &key size column-count synapses-count segments-count)
  (check-type column-count positive-fixnum)
  (check-type synapses-count positive-fixnum)
  (check-type size positive-fixnum)
  (check-type segments-count positive-fixnum)
  (let ((column-size (/ size column-count)))
    (check-type column-size positive-integer))
  (lret ((result (vector-classes:make-data
                  'neuron-layer-weights
                  size
                  :input-size input-size
                  :synapses-count synapses-count
                  :segments-count segments-count
                  :synapses-strength (list segments-count
                                           synapses-count)
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
          (for j from 0 below segments-count)
          (iterate
            (for s from 0 below synapses-count)
            (for index in-vector (shuffle indices))
            (setf (input j s) index)))))))


(defmethod to-declared-layer ((layer layer) (prev integer))
  (apply #'make-weights
         (read-type layer)
         prev
         (read-arguments layer)))


(defmethod to-declared-layer ((layer layer)
                              (prev neuron-layer-weights))
  (apply #'make-weights
         (read-type layer)
         (vector-classes:size prev)
         (read-arguments layer)))


(defmethod declared-layers ((layers list) initial-size)
  (iterate
    (for layer in layers)
    (for prev-layer previous layer initially initial-size)
    (collect (to-declared-layer layer initial-size))))

(in-package #:cl-htm.nl)


(define-constant +empty-vector+ (make-array 0 :element-type 'fixnum)
  :test 'vector=)
(declaim (type (vector fixnum) +empty-vector+))


(defmethod calculate-active-synapses-for-columns
    ((layer neuron-layer)
     (input cl-htm.sdr:sdr)
     (columns neuron-column))
  (declare (optimize (speed 1) (safety 1)
                     (space 0) (debug 3)))
  (nest
   (vector-classes:with-data (((column-input input))
                              columns column neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input j cl-htm.sdr:sdr))
   (vector-classes:with-data (((synaps proximal-synapses-strength))
                              layer neuron neuron-layer))
   (bind ((size (vector-classes:size columns))
          (column-size (truncate (vector-classes:size layer)
                                 size))
          (synapses-count (access-synapses-count layer))
          (active-input-vector (make-array synapses-count
                                           :element-type 'fixnum))
          (result (make-array size :element-type 'non-negative-fixnum))
          ((:dflet count-for-column (column &aux (size 0)))
           (declare (type non-negative-fixnum column size))
           (iterate
             (for i from 0 below synapses-count)
             (for j = (column-input i))
             (unless (zerop (active))
               (setf (aref active-input-vector size) i)
               (incf size)))
           (iterate
             (declare (type fixnum sum neuron))
             (with sum = 0)
             (for neuron from (* column column-size))
             (repeat column-size)
             (iterate
               (for i from 0 below size)
               (for j = (aref active-input-vector i))
               (incf sum (synaps j)))
             (finally (return sum)))))
     (declare (type fixnum synapses-count size))
     (iterate
       (declare (type fixnum i))
       (for i from 0 below size)
       (setf (aref result i) (count-for-column i)))
     result)))


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
        (sort #'<))))


(defmethod select-predictive-neurons
    ((layer neuron-layer)
     (sdr cl-htm.sdr:sdr)
     (training-parameters cl-htm.training:fundamental-parameters)
     (columns neuron-column)
     active-columns
     context)
  (declare (optimize (speed 1) (safety 1) (debug 1) (space 0)))
  (check-type active-columns (array fixnum (*)))
  (bind ((threshold (cl-htm.training:threshold training-parameters))
         (column-size (/ (the fixnum (vector-classes:size layer))
                         (the fixnum (vector-classes:size columns))))
         (previous-active-neurons (cl-htm.training:active-neurons context))
         (result (make-array (truncate column-size 10)
                             :adjustable t
                             :fill-pointer 0
                             :element-type t))
         ((:flet matching-test (active-synapses segment &aux (activity 0)))
          (declare (type segment segment) (type fixnum activity))
          ;; gather active synapses in segment
          (cl-ds.utils:on-ordered-intersection
           (lambda (previous-neuron source.weight)
             (declare (ignore previous-neuron))
             (vector-push-extend source.weight active-synapses)
             (incf activity (weight source.weight)))
           previous-active-neurons
           (segment-source-weight segment)
           :second-key #'source)
          (> activity threshold))
         ((:flet active-segment (segment active-synapses))
          (cl-htm.utils:matching segment
                                 (curry #'matching-test active-synapses)))
         ((:flet gather-neurons (column-index))
          (iterate
            (declare (type fixnum i neuron-index column-start))
            (with column-start = (* column-index column-size))
            (with active-synapses = (vect))
            (for neuron-index from column-start)
            (for i from 0 below column-size)
            (for segment = (distal-segment layer neuron-index))
            (for active-segment = (active-segment segment active-synapses))
            (unless (null active-segment)
              (vector-push-extend (neuron.segment neuron-index
                                                  active-segment
                                                  active-synapses)
                                  result)
              (leave))
            (setf (fill-pointer active-synapses) 0))))
    (declare (type fixnum threshold column-size)
             (type vector result))
    (map nil #'gather-neurons active-columns)
    result))


(defun selecting-the-most-active-neuron (layer columns input)
  (declare (optimize (speed 1) (safety 3) (debug 3) (space 0)))
  (nest
   (vector-classes:with-data (((synapses-strength proximal-synapses-strength))
                              layer neuron-index neuron-layer))
   (vector-classes:with-data (((columns-input input))
                              columns column-index neuron-column))
   (vector-classes:with-data (((active cl-htm.sdr:active-neurons))
                              input input-index cl-htm.sdr:sdr))
   (let* ((column-size (truncate (the fixnum (vector-classes:size layer))
                                (the fixnum (vector-classes:size columns))))
          (synapses-count (access-synapses-count layer))
          (synapses-array (make-array synapses-count
                                      :element-type 'fixnum)))
     (declare (type fixnum column-size synapses-count)))
   (lambda (column-index &aux (segment-pointer 0))
     (declare (type fixnum column-index segment-pointer))
     (iterate
       (declare (type fixnum input-index))
       (for h from 0 below synapses-count)
       (for input-index = (columns-input h))
       (unless (zerop (active))
         (setf (aref synapses-array segment-pointer)
               h)
         (incf segment-pointer)))
     (iterate
       (declare (type fixnum i neuron-index
                      column-start neuron))
       (with column-start = (* column-index column-size))
       (with neuron = 0)
       (for neuron-index from column-start)
       (for i from 0 below column-size)
       (for neuron-signal =
            (iterate
              (declare (type fixnum k i))
              (for i from 0 below segment-pointer)
              (for k = (aref synapses-array i))
              (sum (synapses-strength k))))
       (maximize neuron-signal into maxi)
       (when (eql maxi neuron-signal)
         (setf neuron neuron-index))
       (finally (return neuron))))))


(defmethod select-active-neurons ((layer neuron-layer)
                                  (columns neuron-column)
                                  (input cl-htm.sdr:sdr)
                                  active-columns
                                  predictive-neurons
                                  active-neurons)
  (declare (optimize (debug 3)))
  (check-type predictive-neurons vector)
  (check-type active-columns (simple-array fixnum (*)))
  (check-type active-neurons (array * (*)))
  (setf (fill-pointer active-neurons) 0)
  (let ((column-size (truncate (vector-classes:size layer)
                               (vector-classes:size columns)))
        (push-to-vector (lambda (column neuron)
                          (declare (ignore column))
                          (vector-push-extend (neuron neuron)
                                              active-neurons)))
        (select-and-push (compose (rcurry #'vector-push-extend active-neurons)
                                  (selecting-the-most-active-neuron layer
                                                                    columns
                                                                    input))))
    (declare (type non-negative-fixnum column-size))
    (cl-ds.utils:on-ordered-intersection push-to-vector
                                         active-columns
                                         predictive-neurons
                                         :same #'eql
                                         :on-second-missing select-and-push
     :second-key (lambda (neuron) (truncate (neuron neuron) column-size)))
    active-neurons))


(defun reinforce-segment (p+ p-
                          maximum-weight minimum-weight
                          active-neuron predictive-neuron.segment)
  (declare (ignore active-neuron))
  (bind ((segment (segment predictive-neuron.segment))
         (active-synapses (active-synapses predictive-neuron.segment))
         ((:flet increase-synaps (synaps not-important))
          (declare (ignore not-important))
          (setf (weight synaps)
                (~> (weight synaps)
                    (+ p+)
                    (min maximum-weight))))
         ((:flet decrease-synaps (x))
          (setf (weight x)
                (~> (weight x)
                    (- p-)
                    (max minimum-weight)))))
    (declare (type vector active-synapses))
    ;; segment level
    (cl-ds.utils:on-ordered-intersection #'increase-synaps
                                         (segment-source-weight segment)
                                         active-synapses
                                         :on-second-missing #'decrease-synaps
                                         :same #'eql
                                         :first-key #'source
                                         :second-key #'source
                                         :less #'<)))


(defun update-neurons (active-neurons predictive-neurons parameters)
  (bind ((decay (cl-htm.training:decay parameters))
         (p+ (cl-htm.training:p+ parameters))
         (p- (cl-htm.training:p- parameters))
         (maximum-weight (cl-htm.training:maximum-weight parameters))
         (minimum-weight (cl-htm.training:minimum-weight parameters))
         ((:flet decay (predictive-neuron.segment))
          (~>> predictive-neuron.segment
               segment
               segment-source-weight
               (map nil
                    (lambda (x)
                      (setf (weight x)
                            (~> (weight x) (- decay)
                                (max minimum-weight))))))))
    (declare (type fixnum decay p+ p-
                   maximum-weight minimum-weight))
    (cl-ds.utils:on-ordered-intersection (curry #'reinforce-segment
                                                p+ p- maximum-weight
                                                minimum-weight)
                                         active-neurons
                                         predictive-neurons
                                         :same #'eql
                                         :second-key #'neuron
                                         :on-first-missing #'decay
                                         :on-second-missing #'identity)))


(defmethod update-synapses
    ((parameters cl-htm.training:fundamental-parameters)
     (layer neuron-layer)
     (input cl-htm.sdr:sdr)
     (mode cl-htm.training:train-mode)
     (columns neuron-column)
     active-columns
     predictive-neurons
     active-neurons)
  (declare (optimize (speed 1) (safety 1) (debug 3) (space 0)))
  (check-type predictive-neurons vector)
  (check-type active-columns (simple-array * (*)))
  (check-type active-neurons vector)
  (update-neurons active-neurons predictive-neurons parameters)
  nil)


(defmethod to-effective-layer ((neuron neuron-layer-weights))
  (lret ((result (make 'neuron-layer)))
    (cl-ds.utils:copy-slots (neuron result)
      vector-classes::%size proximal-synapses-strength
      %input-size %synapses-count %segments-count
      %columns distal-segments)))


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
  (declare (optimize (debug 3) (safety 1)))
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
                                     active-columns
                                     context)))
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
      (map nil (lambda (i) (setf (neuron) 1))
           active-columns)
      (setf (cl-htm.sdr:dense-active-neurons sdr) active-columns))))


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
                         &key
                           size column-count
                           synapses-count segments-count)
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
                  :proximal-synapses-count (list synapses-count)
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
          (for s from 0 below synapses-count)
          (for index in-vector (shuffle indices))
          (setf (input s) index))))))


(defmethod to-declared-layer ((layer layer) (prev integer))
  (apply #'make-weights
         (read-type layer)
         prev
         (read-arguments layer)))


(defmethod output-size ((layer layer))
  (~> layer columns vector-classes:size))


(defmethod to-declared-layer ((layer layer)
                              (prev neuron-layer-weights))
  (apply #'make-weights
         (read-type layer)
         (output-size prev)
         (read-arguments layer)))


(defmethod declared-layers ((layers list) initial-size)
  (iterate
    (for layer in layers)
    (for prev-layer previous layer initially initial-size)
    (collect (to-declared-layer layer initial-size))))

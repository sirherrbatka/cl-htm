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
  (cl-ds.utils:select-top (read-column-indices layer)
                          (read-activated-columns-count layer)
                          #'> :key
                          (curry #'aref active-synapses)))


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
         (column-indices (read-column-indices layer))
         (activate-neurons-count (read-activated-neurons-count columns))
         (activated-columns-count (read-activated-columns-count columns))
         (active-synapses-for-columns (calculate-active-synapses-for-columns
                                       layer columns sdr))
         (active-columns (select-active-columns columns
                                                active-synapses-for-columns))
         (predictive-neurons (select-predictive-neurons layer
                                                        columns
                                                        active-columns))
         (active-neurons (select-active-neurons layer
                                                columns
                                                active-columns
                                                prev-data)))
    (cl-htm.sdr:set-active layer active-neurons 1)
    (update-synapses layer columns active-columns prev-data active-neurons)
    predictive-neurons))

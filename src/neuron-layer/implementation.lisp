(in-package #:cl-htm.nl)


(define-constant +empty-vector+ (make-array 0 :element-type 'fixnum)
  :test 'vector=)


*compile-file-truename*
(defmethod activate ((layer neuron-layer)
                     (sdr cl-htm.sdr:sdr)
                     context
                     &optional (prev-data +empty-vector+))
  ;; calculate number of active synapses for each column
  ;; select top active columns
  ;; select predictive neurons
  ;; set active neurons
  ;; finally, return all predictive neurons
  (nest
   (let* ((columns (read-columns layer))
          (column-indices (read-column-indices layer))
          (activate-neurons-count (read-activated-neurons-count columns))
          (activated-columns-count (read-activated-columns-count columns))
          (active-synapses-for-columns (calculate-active-synapses-for-columns
                                        columns sdr))
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
     predictive-neurons)))

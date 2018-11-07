(in-package #:cl-htm.nl)


(define-constant +empty-vector+ (make-array 0 :element-type 'fixnum)
  :test 'vector=)


(defmethod activate ((layer neuron-layer)
                     (sdr cl-htm.sdr:sdr)
                     context
                     &optional (prev-data +empty-vector+))
  (nest
   (let* ((active-neurons (cl-htm.sdr:select-active sdr))
          (columns (read-columns layer))
          (activate-neurons-count (read-activated-neurons-count columns))
          (activated-columns-count (read-activated-columns-count columns))))
   (vector-classes:with-data (((synapses-strength synapses-strength))
                              layer
                              i
                              neuron-layer))
   (vector-classes:with-data (((neuron-input input))
                              columns
                              i
                              neuron-column))
   (cl-htm.sdr:select-active layer)))

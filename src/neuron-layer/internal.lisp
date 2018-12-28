(in-package #:cl-htm.nl)


(defstruct source.weight
  (%source 0 :type (unsigned-byte 16))
  (%weight 0 :type (unsigned-byte 16)))


(defun source (source.weight)
  (source.weight-%source source.weight))


(defun weight (source.weight)
  (source.weight-%weight source.weight))


(defun (setf weight) (new-val source.weight)
  (setf (source.weight-%weight source.weight) new-val))


(defun source.weight (source weight)
  (make-source.weight :%source source
                      :%weight weight))


(defstruct segment
  (source-weight (make-array 0 :element-type 'list) :type (array list (*))))


(defun neuron.segment (neuron segment active-synapses)
  (list neuron segment active-synapses))


(defun active-synapses (neuron.segment)
  (caddr neuron.segment))


(defun neuron (neuron.segment)
  (car neuron.segment))


(defun segment (neuron.segment)
  (cadr neuron.segment))


(defun build-segment (layer synapses-count)
  (let ((layer-size (vector-classes:size layer))
        (source (make-array synapses-count :element-type 'list)))
    (iterate
      (for i from 0 below synapses-count)
      (setf (aref source i) (source.weight
                             (random layer-size)
                             (cl-htm.utils:random-synapses-strength))))
    (make-segment :source-weight (sort source #'< :key #'source))))


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
                 (cl-ds:send-recur (build-segment layer synapses-count)
                                   :i new-i)
                 (cl-ds:send-finish (build-segment layer
                                                   synapses-count))))))))))

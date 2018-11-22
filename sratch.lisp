(defparameter *training-parameters*
  (make-instance 'cl-htm.training:basic-training-parameters
                 :activated-columns-fraction 1/3
                 :threshold 0.6
                 :p- 0.001
                 :p+ 0.003
                 :decay 0.00001))

(defparameter *model* (cl-htm.model:make-model
                       'cl-htm.model:basic-model
                       40
                       *training-parameters*
                       (cl-htm.nl:layers
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 3
                                         :column-count 4
                                         :size 40))))

(defparameter *encoder* (make-instance
                         'cl-htm.model:random-vector-encoder
                         :encoded-length 2
                         :times 1
                         :count 40))

(defparameter *decoder* (make-instance 'cl-htm.model:fundamental-decoder))

(cl-htm.model:insert-point *encoder* *decoder*
                           *model* (make-instance 'cl-htm.model:train-mode)
                           (vector 10)
                           (cl-htm.model:contexts *model*)
                           (cl-htm.model:sdrs *model*))

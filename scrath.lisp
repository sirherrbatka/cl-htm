(defparameter *training-parameters*
  (make-instance 'cl-htm.training:basic-parameters
                 :activated-columns-fraction 1/3
                 :threshold 0.2
                 :p- 0.01
                 :p+ 0.03
                 :decay 0.00001))

(defparameter *encoder* (make-instance
                         'cl-htm.model:random-vector-encoder
                         :encoded-length 3
                         :count 80))

(defparameter *decoder* (make-instance
                         'cl-htm.model:fundamental-decoder))

(defparameter *model* (cl-htm.model:make-model
                       'cl-htm.model:basic-model
                       400
                       *training-parameters*
                       (cl-htm.nl:layers
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 10
                                         :column-count 20
                                         :size 800)
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 10
                                         :column-count 20
                                         :size 400)
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 10
                                         :column-count 20
                                         :size 200))
                       :input *encoder*
                       :decoder *decoder*))

(require :sb-sprof)
(progn
  (sb-sprof:reset)
  (sb-sprof:start-profiling)
  (time
   (cl-htm.model:train *model*
                       (cl-ds:xpr (:i 100000)
                         (unless (zerop i)
                           (cl-ds:send-recur (vector 1 2 3)
                                             :i (1- i))))))
  (sb-sprof:stop-profiling))

(defparameter *training-parameters*
  (make-instance 'cl-htm.training:basic-parameters
                 :activated-columns-fraction 0.02
                 :threshold 80
                 :p- 10
                 :p+ 30
                 :decay 5))

(defparameter *encoder* (make-instance
                         'cl-htm.model:random-vector-encoder
                         :encoded-length 3
                         :count 40))

(defparameter *decoder* (make-instance
                         'cl-htm.model:fundamental-decoder))

(defparameter *model* (cl-htm.model:make-model
                       'cl-htm.model:basic-model
                       2000
                       *training-parameters*
                       (cl-htm.nl:layers
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 20
                                         :column-count 100
                                         :size 2000)
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 20
                                         :column-count 1000
                                         :size 2000)
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 20
                                         :column-count 1000
                                         :size 2000))
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

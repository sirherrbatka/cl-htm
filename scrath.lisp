(defparameter *training-parameters*
  (make-instance 'cl-htm.training:basic-parameters
                 :activated-columns-fraction 0.02
                 :maximum-weight 1000
                 :threshold 200
                 :p- 5
                 :p+ 10
                 :decay 2))

(defparameter *encoder* (make-instance
                         'cl-htm.model:random-vector-encoder
                         :encoded-length 2
                         :count 80))

(defparameter *decoder* (cl-htm.model:make-vector-decoder
                         'eql
                         2
                         0.2))

(defparameter *model* (cl-htm.model:make-model
                       'cl-htm.model:basic-model
                       4000
                       *training-parameters*
                       (cl-htm.nl:layers
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 128
                                         :segments-count 128
                                         :column-count 100
                                         :size 4000)
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 128
                                         :segments-count 128
                                         :column-count 100
                                         :size 4000)
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 128
                                         :segments-count 128
                                         :column-count 100
                                         :size 4000)
                        (cl-htm.nl:layer 'cl-htm.nl:neuron-layer-weights
                                         :synapses-count 128
                                         :segments-count 128
                                         :column-count 100
                                         :size 4000))
                       :input *encoder*
                       :decoder *decoder*))

(cl-htm.model:train *model*
                    (cl-ds:xpr (:i 10)
                      (unless (zerop i)
                        (cl-ds:send-recur (vector 1 2 3)
                                          :i (1- i)))))

(cl-htm.model:adapt *model* (cl-ds:xpr (:i 100)
                              (unless (zerop i)
                                (cl-ds:send-recur (vector 1 2 3)
                                                  :i (1- i)))))

(defparameter *predictions*
  (cl-htm.model:predict *model*
                        (cl-ds:xpr (:i 50)
                          (unless (zerop i)
                            (cl-ds:send-recur (vector 1 2 3)
                                              :i (1- i))))))

(defparameter *predictions-vector*
  (cl-ds.alg:to-vector *predictions*))

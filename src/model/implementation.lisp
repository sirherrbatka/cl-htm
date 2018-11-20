(in-package #:cl-htm.model)


(let ((no-training (make 'cl-htm.training:empty-training-parameters)))
  (defmethod parameters ((model fundamental-model)
                         (mode predict-mode))
    no-training))


(defmethod parameters ((model basic-model)
                       (mode train-mode))
  (read-training-parameters model))


(defmethod parameters ((model fundamental-model)
                       (mode predict-mode))
  (make 'cl-htm.training:empty-training-parameters))


(defmethod contexts ((model basic-model))
  (~>> model read-layers
       (mapcar #'cl-htm.nl:context)))


(defmethod reset-model ((model basic-model)
                        (contexts list)
                        sdrs)
  (map nil #'cl-htm.training:reset-context contexts)
  (map nil #'cl-htm.sdr:clear-all-active sdrs)
  model)


(defmethod activate ((model basic-model)
                     (mode fundamental-mode)
                     (contexts list)
                     parameters
                     sdrs)
  (iterate
    (with sdrs  = (rest sdrs))
    (for sdr in sdrs)
    (for input previous sdr
         initially (first sdrs))
    (for context in contexts)
    (cl-htm.nl:activate sdr input context parameters)
    (finally (return model))))


(defmethod sdrs ((model basic-model))
  (cons (vector-classes:make-data 'cl-htm.sdr:sdr
                                  (read-input-sdr-size model))
        (mapcar #'cl-htm.nl:to-sdr (read-layers model))))


(defmethod input-sdr ((model basic-model) (sdrs list))
  (first sdrs))


(defmethod output-sdr ((model basic-model) (sdrs list))
  (last sdrs))


(defmethod contexts ((model basic-model))
  (~>> model read-layers
       (mapcar (rcurry #'context model))))


(defmethod context ((layer cl-htm.nl:neuron-layer-weights) (model basic-model) )
  (make 'cl-htm.training:basic-training-context))


(defmethod pass-to-decoder ((decoder fundamental-decoder)
                            (model fundamental-model)
                            (mode predict-mode)
                            data-point
                            sdrs)
  (decode-sdr decoder (output-sdr model sdrs)))


(defmethod insert-point ((input fundamental-input)
                         (decoder fundamental-decoder)
                         (model fundamental-model)
                         data-point
                         mode
                         contexts
                         sdrs)
  (unwind-protect
       (iterate
         (with initial-data = data-point)
         (with destination  = (input-sdr model sdrs))
         (with parameters   = (parameters model mode))
         (while (more-data-p input data-point))
         (setf data-point (encode-data-point input
                                             destination
                                             data-point))
         (activate model mode contexts parameters sdrs)
         (finally (return (pass-to-decoder decoder model mode
                                           initial-data sdrs))))
    (reset-model model contexts sdrs)))


(defmethod predict ((input fundamental-input)
                    (decoder fundamental-decoder)
                    (model fundamental-model)
                    data)
  (let ((mode (make 'predict-mode))
        (sdrs (sdrs model))
        (contexts (contexts model)))
    (cl-ds.alg:on-each
     (lambda (data-point)
       (insert-point input decoder model mode
                     data-point contexts sdrs))
     data)))


(defmethod train ((input fundamental-input)
                  (decoder fundamental-decoder)
                  (model fundamental-model)
                  data)
  (let ((mode (make 'train-mode))
        (sdrs (sdrs model))
        (contexts (contexts model)))
    (cl-ds:traverse
     (lambda (data-point)
       (insert-point input decoder model mode
                     data-point contexts sdrs))
     data))
  model)

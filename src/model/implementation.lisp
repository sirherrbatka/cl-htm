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
  (~>> (read-layers model)
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
    (with input = (first sdrs))
    (with sdrs  = (rest sdrs))
    (for sdr in sdrs)
    (for input previous sdr
         initially input)
    (for context in contexts)
    (cl-htm.nl:activate sdr input context parameters)
    (finally (return model))))

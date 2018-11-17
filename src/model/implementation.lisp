(in-package #:cl-htm.model)


(let ((no-training (make 'cl-htm.training:empty-training-parameters)))
  (defmethod parameters ((model fundamental-model)
                         (mode predict-mode))
    no-training))


(defmethod parameters ((model basic-model)
                       (mode train-mode))
  (read-training-parameters model))

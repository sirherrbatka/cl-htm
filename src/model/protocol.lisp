(in-package #:cl-htm.model)


(defvar *no-training* (make 'cl-htm.training:empty-training-parameters))

(defgeneric encode-data-point (input destination data-point))

(defgeneric more-data-p (input data-point))

(defgeneric encode-whole-data (input destination data-point)
  (:method ((input fundamental-input)
            (destination cl:sequence)
            data-point)
    (iterate
      (while (more-data-p input data-point))
      (setf data-point (encode-data-point input
                                          destination
                                          data-point)))))

(defgeneric reset-model (model))

(defgeneric activate (model parameters))

(defgeneric insert-point (input
                          model
                          data-point
                          training-parameters)
  (:method ((input fundamental-input)
            (model fundamental-model)
            data-point
            training-parameters)
    (encode-whole-data input
                       (input-sdrs model)
                       data-point)
    (activate model training-parameters)))

(defgeneric training-parameters (model))

(defgeneric train-point (input decoder model data-point)
  (:method ((input fundamental-input)
            (decoder fundamental-decoder)
            (model fundamental-model)
            data-point)
    (unwind-protect
         (insert-point input model data-point
                       (training-parameters model))
      (reset-model model))))

(defgeneric output-sdrs (model))

(defgeneric input-sdrs (model))

(defgeneric decode-sdrs (decoder sdrs))

(defgeneric decode (decoder model)
  (:method ((decoder fundamental-decoder)
            (model fundamental-model))
    (decode-sdrs decoder
                 (output-sdrs model))))

(defgeneric predict-point (input decoder model data-point)
  (:method ((input fundamental-input)
            (decoder fundamental-decoder)
            (model fundamental-model)
            data-point)
    (unwind-protect
         (progn
           (insert-point input model data-point *no-training*)
           (decode decoder model))
      (reset-model model))))

(defgeneric predict (input decoder model data)
  (:method ((input fundamental-input)
            (decoder fundamental-decoder)
            (model fundamental-model)
            data)
    (cl-ds.alg:on-each
     (lambda (data-point)
       (predict-point input decoder model data-point))
     data)))

(defgeneric train (input decoder model data)
  (:method ((input fundamental-input)
            (decoder fundamental-decoder)
            (model fundamental-model)
            data)
    (cl-ds:traverse
     (lambda (data-point)
       (train-point input decoder model data-point))
     data)))

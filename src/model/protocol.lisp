(in-package #:cl-htm.model)


(defgeneric encode-data-point (input destination data-point))

(defgeneric more-data-p (input data-point))

(defgeneric reset-model (model contexts))

(defgeneric activate (model mode contexts parameters))

(defgeneric parameters (model mode))

(defgeneric contexts (model))

(defgeneric output-sdrs (model))

(defgeneric input-sdrs (model))

(defgeneric decode-sdrs (decoder sdrs))

(defgeneric pass-to-decoder (decoder model mode data-point)
  (:method ((decoder fundamental-decoder)
            (model fundamental-model)
            (mode predict-mode)
            data-point)
    (decode-sdrs decoder
                 (output-sdrs model))))

(defgeneric insert-point (input decoder model mode data-point contexts)
  (:method ((input fundamental-input)
            (decoder fundamental-decoder)
            (model fundamental-model)
            data-point
            mode
            contexts)
    (unwind-protect
         (iterate
           (with initial-data = data-point)
           (with destination  = (input-sdrs model))
           (with parameters   = (parameters model mode))
           (while (more-data-p input data-point))
           (setf data-point (encode-data-point input
                                               destination
                                               data-point))
           (activate model mode contexts parameters)
           (finally (return (pass-to-decoder decoder
                                             model
                                             mode
                                             initial-data))))
      (reset-model model contexts))))

(defgeneric predict (input decoder model data)
  (:method ((input fundamental-input)
            (decoder fundamental-decoder)
            (model fundamental-model)
            data)
    (let ((mode (make 'predict-mode))
          (contexts (contexts model)))
      (cl-ds.alg:on-each
       (lambda (data-point)
         (insert-point input decoder model mode data-point contexts))
       data))))

(defgeneric train (input decoder model data)
  (:method ((input fundamental-input)
            (decoder fundamental-decoder)
            (model fundamental-model)
            data)
    (let ((mode (make 'train-mode))
          (contexts (contexts model)))
      (cl-ds:traverse
       (lambda (data-point)
         (insert-point input decoder model mode data-point contexts))
       data))
    model))

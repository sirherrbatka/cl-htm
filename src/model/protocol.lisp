(in-package #:cl-htm.model)


(defgeneric encode-data-point (input destination data-point))

(defgeneric more-data-p (input data-point))

(defgeneric reset-model (model contexts sdrs))

(defgeneric activate (model mode contexts parameters sdrs))

(defgeneric sdrs (model))

(defgeneric parameters (model mode))

(defgeneric contexts (model))

(defgeneric output-sdr (model sdrs))

(defgeneric input-sdr (model sdrs))

(defgeneric decode-sdr (decoder sdr))

(defgeneric pass-to-decoder (decoder model mode data-point sdrs)
  (:method ((decoder fundamental-decoder)
            (model fundamental-model)
            (mode predict-mode)
            data-point
            sdrs)
    (decode-sdr decoder (output-sdr model sdrs))))

(defgeneric insert-point (input decoder model mode data-point contexts sdrs)
  (:method ((input fundamental-input)
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
           (finally (return (pass-to-decoder decoder
                                             model
                                             mode
                                             initial-data
                                             sdrs))))
      (reset-model model contexts sdrs))))

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
          (sdrs (sdrs model))
          (contexts (contexts model)))
      (cl-ds:traverse
       (lambda (data-point)
         (insert-point input decoder model mode data-point contexts sdrs))
       data))
    model))

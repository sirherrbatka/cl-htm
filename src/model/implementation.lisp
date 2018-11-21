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
                        sdrs
                        contexts)
  (cl-htm.sdr:clear-all-active (first sdrs))
  (iterate
    (for sdr in (rest sdrs))
    (for context in contexts)
    (cl-htm.sdr:set-active sdr
                           (cl-htm.training:active-neurons context)
                           0))
  model)


(defmethod activate ((model basic-model)
                     (mode fundamental-mode)
                     (contexts list)
                     parameters
                     sdrs)
  (iterate
    (with sdrs = (rest sdrs))
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


(defmethod context ((layer cl-htm.nl:neuron-layer-weights)
                    (model basic-model))
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
         (while (more-data-p input mode data-point))
         (setf data-point (encode-data-point input
                                             destination
                                             data-point))
         (activate model mode contexts parameters sdrs)
         (finally (return (pass-to-decoder decoder model mode
                                           initial-data sdrs))))
    (reset-model model sdrs contexts)))


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


(define-constant +long-prime+ 4294967311)


(defun hashval (hashes width j hash)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (* 2)) hashes)
           (type non-negative-fixnum width j hash))
  (~> (aref hashes j 0)
      (* hash)
      (ldb (byte 32 0) _)
      (+ (aref hashes j 1))
      (ldb (byte 32 0) _)
      (rem +long-prime+)
      (rem width)))


(defun make-hash-array (count)
  (lret ((result (make-array (list count 2) :element-type 'fixnum)))
    (map-into (cl-ds.utils:unfold-table result)
              (curry #'random most-positive-fixnum))))


(defmethod initialize-instance :after
    ((instance random-symbol-encoder) &key &allow-other-keys)
  (setf (slot-value instance '%hashes)
        (make-hash-array (read-count instance))))


(defmethod encode-data-point ((input random-symbol-encoder)
                              (destination cl-htm.sdr:sdr)
                              data-point)
  (vector-classes:with-data (((in cl-htm.sdr:active-neurons))
                             input
                             j
                             cl-htm.sdr:sdr)
    (iterate
      (with hashes = (read-hashes input))
      (with hash-function = (read-hash-function input))
      (with hash = (~> hash-function
                       (funcall data-point)
                       (ldb (byte 32 0) _)))
      (with size = (array-dimension in 0))
      (with count = (read-count input))
      (for i from 0 below count)
      (for j = (hashval hashes size i hash))
      (setf (in) 1))
    nil))


(defmethod more-data-p ((input random-symbol-encoder)
                        (mode fundamental-mode)
                        data-point)
  (not (null data-point)))


(defmacro ensure-data-wrapping (data-point)
  `(when (typep ,data-point 'vector)
     (setf ,data-point (cons ,data-point 0))))


(defmethod more-data-p ((input random-vector-encoder)
                        (mode fundamental-mode)
                        data-point)
  (ensure-data-wrapping data-point)
  (< (the fixnum (cdr data-point))
     (the fixnum (read-encoded-duration input))))


(defmethod more-data-p ((input random-vector-encoder)
                        (mode train-mode)
                        data-point)
  (ensure-data-wrapping data-point)
  (< (the fixnum (cdr data-point))
     (the fixnum (length (car data-point)))))


(defmethod encode-data-point ((input random-symbol-encoder)
                              (destination cl-htm.sdr:sdr)
                              data-point)
  (bind (((data . index) (car data-point))
         (value (aref data index)))
    (call-next-method input destination value)
    (cons data (1+ index))))


(defmethod make-model
    ((model-class (eql 'basic-model))
     layers
     input-size
     (training-parameters cl-htm.training:fundamental-training-parameters))
  (check-type input-size non-negative-integer)
  (make 'basic-model
        :layers (cl-htm.nl:effective-layers layers)
        :input-size input-size
        :training-parameters training-parameters))

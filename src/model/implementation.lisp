(in-package #:cl-htm.model)


(defmethod parameters ((model basic-model))
  (read-training-parameters model))


(defmethod contexts ((model basic-model))
  (~>> model read-layers
       (mapcar #'cl-htm.nl:context)))


(defmethod reset-model ((model basic-model)
                        sdrs
                        contexts)
  (~> sdrs first cl-htm.sdr:clear-all-active)
  (map nil #'cl-htm.training:reset-context contexts)
  model)


(defmethod activate ((model basic-model)
                     (mode cl-htm.training:fundamental-mode)
                     (contexts list)
                     parameters
                     sdrs)
  (iterate
    (with layers = (rest sdrs))
    (with input = (first sdrs))
    (for layer in layers)
    (for context in contexts)
    (cl-htm.nl:activate layer input context parameters mode)
    (finally (return model))))


(defmethod layers ((model basic-model))
  (let* ((layers (mapcar #'cl-htm.nl:to-effective-layer (read-layers model)))
         (max-size (reduce #'max layers :key #'vector-classes:size
                           :initial-value (read-input-sdr-size model))))
    (cons (vector-classes:make-data 'cl-htm.sdr:sdr max-size)
          layers)))


(defmethod input/output-sdr ((model basic-model) (sdrs list))
  (first sdrs))


(defmethod contexts ((model basic-model))
  (~>> model read-layers
       (mapcar (rcurry #'context model))))


(defmethod context ((layer cl-htm.nl:neuron-layer-weights)
                    (model basic-model))
  (make 'cl-htm.training:basic-context))


(defmethod pass-to-decoder ((decoder fundamental-decoder)
                            (model fundamental-model)
                            (mode cl-htm.training:train-mode)
                            data-point
                            sdrs
                            contexts)
  nil)


(defmethod pass-to-decoder ((decoder fundamental-discreete-decoder)
                            (model fundamental-model)
                            (mode cl-htm.training:predict-mode)
                            data-point
                            sdrs
                            contexts)
  (~>> decoder read-outputs
       (prediction (last-elt contexts))))


(defmethod pass-to-decoder ((decoder fundamental-discreete-decoder)
                            (model fundamental-model)
                            (mode cl-htm.training:adapt-mode)
                            data-point
                            sdrs
                            contexts)
  (~> decoder read-outputs (add-data-point (last-elt contexts) data-point)))


(defmethod pass-to-decoder ((decoder fundamental-vector-decoder)
                            (model fundamental-model)
                            (mode cl-htm.training:fundamental-mode)
                            data-point
                            sdrs
                            contexts)
  (call-next-method decoder model mode
                    (aref data-point (read-prediction-index decoder))
                    sdrs
                    contexts))


(defmethod insert-point ((input fundamental-input)
                         (decoder fundamental-decoder)
                         (model fundamental-model)
                         mode
                         data-point
                         contexts
                         sdrs)
  (declare (optimize (debug 3)))
  (let ((destination (input/output-sdr model sdrs))
        (parameters (parameters model))
        (initial-data data-point))
    (iterate
      (while (more-data-p input mode data-point))
      (setf data-point (encode-data-point input destination data-point))
      (activate model mode contexts parameters sdrs)
      (when (more-data-p input mode data-point)
        (cl-htm.sdr:clear-all-active destination))
      (finally (return
                 (prog1 (pass-to-decoder decoder model mode
                                         initial-data sdrs
                                         contexts)
                   (reset-model model sdrs contexts)))))))


(defmethod predict ((model fundamental-model)
                    data
                    &key (input (input model)) (decoder (decoder model)))
  (let ((mode (make 'cl-htm.training:predict-mode))
        (sdrs (layers model))
        (contexts (contexts model)))
    (cl-ds.alg:on-each
     (lambda (data-point)
       (insert-point input decoder model mode
                     data-point contexts sdrs))
     data)))


(defmethod train ((model fundamental-model)
                  data
                  &key (input (input model)) (decoder (decoder model)))
  (let* ((mode (make 'cl-htm.training:train-mode))
         (queue (lparallel.queue:make-queue :fixed-capacity 32))
         (thread
           (bt:make-thread
            (lambda ()
              (~>>
               data
               (make-instance 'cl-ds:chunked-range :chunk-size 32000
                                                   :original-range _)
               (cl-ds:traverse (lambda (chunk)
                                 (lparallel.queue:push-queue
                                  (lparallel:future
                                    (cl-ds:traverse
                                     (lambda (data
                                              &aux
                                                (contexts (contexts model))
                                                (sdrs (layers model)))
                                       (insert-point input decoder model mode
                                                     data contexts sdrs))
                                     chunk)
                                    t)
                                  queue))))
              (lparallel.queue:push-queue nil queue)))))
    (iterate
      (for v = (lparallel:force (lparallel.queue:pop-queue queue)))
      (while v))
    (bt:join-thread thread))
  model)


(defmethod adapt ((model fundamental-model)
                  data
                  &key (input (input model)) (decoder (decoder model)))
  (let* ((mode (make 'cl-htm.training:adapt-mode))
         (queue (lparallel.queue:make-queue :fixed-capacity 32))
         (thread
           (bt:make-thread
            (lambda ()
              (~>>
               data
               (make-instance 'cl-ds:chunked-range :chunk-size 32000
                                                   :original-range _)
               (cl-ds:traverse (lambda (chunk)
                                 (lparallel.queue:push-queue
                                  (lparallel:future
                                    (cl-ds:traverse
                                     (lambda (data
                                              &aux
                                                (contexts (contexts model))
                                                (sdrs (layers model)))
                                       (insert-point input decoder model mode
                                                     data contexts sdrs))
                                     chunk)
                                    t)
                                  queue))))
              (lparallel.queue:push-queue nil queue)))))
    (iterate
      (for v = (lparallel:force (lparallel.queue:pop-queue queue)))
      (while v))
    (bt:join-thread thread))
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
                             destination
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
                        (mode cl-htm.training:fundamental-mode)
                        data-point)
  (not (null data-point)))


(defmacro ensure-data-wrapping (data-point)
  `(when (typep ,data-point 'vector)
     (setf ,data-point (cons ,data-point 0))))


(defmethod more-data-p ((input random-vector-encoder)
                        (mode cl-htm.training:fundamental-mode)
                        data-point)
  (ensure-data-wrapping data-point)
  (< (the fixnum (cdr data-point))
     (the fixnum (read-encoded-length input))))


(defmethod more-data-p ((input random-vector-encoder)
                        (mode cl-htm.training:train-mode)
                        data-point)
  (ensure-data-wrapping data-point)
  (< (the fixnum (cdr data-point))
     (the fixnum (length (car data-point)))))


(defmethod encode-data-point ((input random-vector-encoder)
                              (destination cl-htm.sdr:sdr)
                              data-point)
  (ensure-data-wrapping data-point)
  (bind (((data . index) data-point)
         (value (aref data index)))
    (call-next-method input destination value)
    (cons data (1+ index))))


(defmethod make-model
    ((model-class (eql 'basic-model))
     input-size
     (training-parameters cl-htm.training:fundamental-parameters)
     layers
     &key input decoder)
  (check-type input-size non-negative-integer)
  (let* ((layers (cl-htm.nl:declared-layers layers input-size))
         (output-size (~> layers last-elt vector-classes:size)))
    (make 'basic-model
          :layers layers
          :input input
          :output-size output-size
          :decoder decoder
          :input-sdr-size input-size
          :training-parameters training-parameters)))


(defun make-vector-decoder (hash-table-test prediction-index close-limit)
  (assert (member hash-table-test '(eq eql equal)))
  (make-instance 'fundamental-vector-decoder
                 :prediction-index prediction-index
                 :outputs (make-instance
                           'all-outputs
                           :test hash-table-test
                           :close-limit close-limit)))

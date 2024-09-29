(in-package :utaticl.core)

(defmethod (setf .latency-pdc) :around (value (self connection))
  (when (/= (.latency-pdc self) value)
    (call-next-method)
    (setf (.pdc-buffer self)
          ;; 2ch
          (make-instance 'ring-buffer :size (* value 2)))))

(defmethod process ((self connection))
  (when (.start-p (.from self))
    (let* ((from-process-data (.from-process-data self))
           (from-buses (.outputs from-process-data))
           (to-process-data *process-data*)
           (to-buses (.inputs to-process-data))
           (from-bus-index (.from-bus-index self))
           (to-bus-index (.to-bus-index self))
           (from-bus (nth from-bus-index from-buses))
           (to-bus (nth to-bus-index to-buses)))
      (loop for channel-index below 2
            for from-channel = (buffer-at from-bus channel-index)
            for to-channel = (buffer-at to-bus channel-index)
            for from-const-p = (const-get from-bus channel-index)
            for to-const-p = (const-get to-bus channel-index)
            for to-first-value = (cffi:mem-aref to-channel :float 0)
            do (loop for i below (.frames-per-buffer *config*)
                     for from-index = (if from-const-p 0 i)
                     for value-from = (let ((value (cffi:mem-aref from-channel :float from-index)))
                                        (if (plusp (.latency-pdc self))
                                            (prog1
                                                (ring-buffer-pop (.pdc-buffer self))
                                              (ring-buffer-push (.pdc-buffer self) value))
                                            value))
                     do (setf (cffi:mem-aref to-channel :float i)
                              (+ value-from
                                 (if to-const-p
                                     to-first-value
                                     (cffi:mem-aref to-channel :float i)))))
               (const-set to-bus channel-index nil)))))

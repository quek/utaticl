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
           (to-bus-index (.to-bus-index self)))
      (loop for channel-index below 2
            for from-channel = (buffer from-buses from-bus-index channel-index)
            for to-channel = (buffer to-buses to-bus-index channel-index)
            for from-silent-p = (silence-flags from-buses from-bus-index channel-index)
            for to-silent-p = (silence-flags to-buses to-bus-index channel-index)
            unless from-silent-p
              do (loop for i below (.frames-per-buffer *config*)
                       for value-from = (let ((value (cffi:mem-aref from-channel :float i)))
                                          (if (plusp (.latency-pdc self))
                                              (prog1
                                                (ring-buffer-pop (.pdc-buffer self))
                                                (ring-buffer-push (.pdc-buffer self) value))
                                              value))
                       do (setf (cffi:mem-aref to-channel :float i)
                                (if to-silent-p
                                    value-from
                                    (+ value-from
                                       (cffi:mem-aref to-channel :float i)))))
                 (setf (silence-flags to-buses to-bus-index channel-index) nil)))))

(in-package :utaticl.core)

;;(portaudio::print-devices)

(defmethod initialize-instance :after ((self audio-device) &key)
  (let ((handle (.handle self)))
    (sb-ext:finalize self
                     (lambda ()
                       (cffi:foreign-free handle)))))

(cffi:defbitfield (pa-stream-callback-flags :unsigned-long)
  (:input-underflow #x00000001)
  (:input-overflow #x00000002)
  (:output-underflow #x00000004)
  (:output-overflow #x00000008)
  (:priming-output #x00000010))

(defmethod open-audio-device ((self audio-device))
  (if (and (.audio-device-api *config*)
           (.audio-device-name *config*))
      (let* ((output-parameters (pa::make-stream-parameters))
             (latency .0d0)
             (device-index
               (loop for i of-type fixnum below (pa:get-device-count)
                     for device-info = (pa:get-device-info i)
                       thereis (and
                                (equal (pa:host-api-info-name
                                        (pa:get-host-api-info (pa:device-info-host-api device-info)))
                                       (.audio-device-api *config*))
                                (equal (pa:device-info-name device-info)
                                       (.audio-device-name *config*))
                                (setf latency (pa:device-info-default-low-output-latency device-info))
                                i))))
        (if device-index
            (progn
              (setf (pa::stream-parameters-device output-parameters) device-index)
              (setf (pa:stream-parameters-channel-count output-parameters) (.output-channels self)
                    (pa:stream-parameters-sample-format output-parameters) (.sample-format self)
                    (pa::stream-parameters-suggested-latency output-parameters) latency)
              (setf (.stream self)
                    (progn
                      (pa::raise-if-error
                       (pa::%open-stream
                        (.handle self)
                        nil
                        output-parameters
                        (.sample-rate *config*)
                        (.frames-per-buffer *config*)
                        0
                        (cffi:callback audio-callback)
                        (cffi:null-pointer)))
                      (make-instance
                       'pa:pa-stream
                       :handle (cffi:mem-ref (.handle self) :pointer)
                       :input-sample-format (.sample-format self)
                       :input-channels (if (zerop (the fixnum (.input-channels self)))
                                           nil
                                           (.input-channels self))
                       :output-sample-format (.sample-format self)
                       :output-channels (if (zerop (the fixnum (.output-channels self)))
                                            nil
                                            (.output-channels self))
                       :frames-per-buffer (.frames-per-buffer *config*))))
              t)
            nil))
      nil))

(defmethod close-audio-device ((self audio-device))
  (when (.stream self)
    (stop-audio-device self)
    (pa:close-stream (.stream self))
    (setf (.stream self) nil)))

(defmethod start-audio-device ((self audio-device))
  (when (.stream self)
    (unless (.processing self)
      (setf (.processing self) t)
      (pa:start-stream (.stream self)))))

(defmethod stop-audio-device ((self audio-device))
  (when (.processing self)
    (setf (.processing self) nil)
    (pa::stop-stream (.stream self))))

(defun audio-loop (output-buffer)
  (let ((audio-device (.audio-device *app*)))
    (statistic-enter audio-device)
    (process *app*)

    (let ((master-track (.master-track (car (.projects *app*)))))
      (loop with in-left = (buffer (.outputs (.process-data master-track)) 0 0)
            with in-right = (buffer (.outputs (.process-data master-track)) 0 1)
            for i below (.frames-per-buffer *config*)
            do (setf (cffi:mem-aref output-buffer :float (* i 2))
                     (cffi:mem-aref in-left :float i))
               (setf (cffi:mem-aref output-buffer :float (1+ (* i 2)))
                     (cffi:mem-aref in-right :float i))))

      (statistic-leave audio-device)))


(cffi:defcallback audio-callback :int ((input-buffer :pointer)
                                       (output-buffer :pointer)
                                       (frame-per-buffer :unsigned-long)
                                       (time-info :pointer)
                                       (status-flags pa-stream-callback-flags)
                                       (user-data :pointer))
  (declare (optimize (speed 3) (safety 0))
           (ignore input-buffer time-info status-flags user-data
                   frame-per-buffer))
  ;; sb-sys:without-gcing しなくてもたいして変わらない気もする
  (sb-sys:without-gcing
    (audio-loop output-buffer)
    0))

(defun statistic-enter (self)
  (let* ((now (get-internal-real-time))
         (delta (- now (.statistic-leave-time self))))
    (setf (.statistic-enter-time self) now)
    (incf (.statistic-total-interval-time self) delta)
    (setf (.statistic-min-interval-time self)
          (min (.statistic-min-interval-time self) delta))
    (setf (.statistic-max-interval-time self)
          (max (.statistic-max-interval-time self) delta))))

(defun statistic-leave (self)
  (let* ((now (get-internal-real-time))
         (delta (- now (.statistic-enter-time self))))
    (setf (.statistic-leave-time self) now)
    (incf (.statistic-total-process-time self) delta)
    (setf (.statistic-min-process-time self)
          (min (.statistic-min-process-time self) delta))
    (setf (.statistic-max-process-time self)
          (max (.statistic-max-process-time self) delta))
    (when (<= (* (/ (.sample-rate *config*) (.frames-per-buffer *config*)) 10)
              (incf (.statistic-count self)))
      (let ((cpu (* (/ (.statistic-total-process-time self)
                       (+ (.statistic-total-process-time self)
                          (.statistic-total-interval-time self)))
                    100))
            (process-avg (/ (.statistic-total-process-time self)
                            (.statistic-count self)
                            internal-time-units-per-second))
            (process-min (/ (.statistic-min-process-time self) internal-time-units-per-second))
            (process-max (/ (.statistic-max-process-time self) internal-time-units-per-second))
            (interval-avg (/ (.statistic-total-interval-time self)
                             (.statistic-count self)
                             internal-time-units-per-second))
            (interval-min (/ (.statistic-min-interval-time self) internal-time-units-per-second))
            (interval-max (/ (.statistic-max-interval-time self) internal-time-units-per-second)))
        (setf (.statistic-summary self)
              (format nil "~&AUDIO CPU ~f% PROCESS ~fs ~fs ~fs INTERVAL ~fs ~fs ~fs FRAME ~,4fs "
                      cpu
                      process-avg process-min process-max
                      interval-avg interval-min interval-max
                      (/ (.frames-per-buffer *config*) (.sample-rate *config*)))))
      (setf (.statistic-total-process-time self) 0)
      (setf (.statistic-min-process-time self) most-positive-fixnum)
      (setf (.statistic-max-process-time self) 0)
      (setf (.statistic-total-interval-time self) 0)
      (setf (.statistic-min-interval-time self) most-positive-fixnum)
      (setf (.statistic-max-interval-time self) 0)
      (setf (.statistic-count self) 0))))



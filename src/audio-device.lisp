(in-package :dgw)

;;(portaudio::print-devices)

(defparameter *sample-rate* 48000.0d0)
(defparameter *frames-per-buffer* 1024)

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
             (device-index
               (loop for i of-type fixnum below (pa:get-device-count)
                     for device-info = (pa:get-device-info i)
                       thereis (and
                                (equal (pa:host-api-info-name
                                        (pa:get-host-api-info (pa:device-info-host-api device-info)))
                                       (.audio-device-api *config*))
                                (equal (pa:device-info-name device-info)
                                       (.audio-device-name *config*))
                                i))))
        (if device-index
            (progn
              (setf (pa::stream-parameters-device output-parameters) device-index)
              (setf (pa:stream-parameters-channel-count output-parameters) (.output-channels self)
                    (pa:stream-parameters-sample-format output-parameters) (.sample-format self)
                    (pa::stream-parameters-suggested-latency output-parameters) 0.0d0) ;TODO
              (setf (.stream self)
                    (progn
                      (pa::raise-if-error
                       (pa::%open-stream
                        (.handle self)
                        nil
                        output-parameters
                        *sample-rate*
                        *frames-per-buffer*
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
                       :frames-per-buffer *frames-per-buffer*)))
              t)
            nil))
      nil))

(defmethod close-audio-device ((self audio-device))
  (stop-audio-device self)
  (when (.stream self)
    (pa:close-stream (.stream self))
    (setf (.stream self) nil)))

(defmethod start-audio-device ((self audio-device))
  (unless (.processing self)
    (setf (.processing self) t)
    (pa:start-stream (.stream self))))

(defmethod stop-audio-device ((self audio-device))
  (when (.processing self)
    (setf (.processing self) nil)
    (pa::stop-stream (.stream self))))

(defun write-master-buffer (buffer)
  (flet ((limit (value)
           (cond ((< 1.0 value)
                  (warn "音大きすぎ ~a" value)
                  1.0)
                 ((< value -1.0)
                  (warn "音大きすぎ ~a" value)
                  -1.0)
                 (t value))))
    (let* ((left (car (.master-buffer *audio*)))
           (right (cadr (.master-buffer *audio*)))
           (volume 1.0))
      (loop for i below *frames-per-buffer*
            do (setf (cffi:mem-aref buffer :float (* i 2))
                     (limit (* (aref left i) volume)))
               (setf  (cffi:mem-aref buffer :float (1+ (* i 2)))
                      (limit (* (aref right i) volume)))
               (setf (aref left i) 0.0
                     (aref right i) 0.0)))))

(defun audio-loop ()
  (statistic-enter)
  (process *app*)

  ;; TODO かんぜんに暫定
  (let ((master-track (.master-track (car (.projects *app*)))))
    (loop for channel-index below 2
          for in = (buffer (.inputs (.process-data master-track)) 0 channel-index)
          for out = (nth channel-index (.master-buffer *audio*))
          do (loop for i below *frames-per-buffer*
                   do (setf (aref out i)
                            (cffi:mem-aref in :float i)))))

  (statistic-leave))


(cffi:defcallback audio-callback :int ((input-buffer :pointer)
                                       (output-buffer :pointer)
                                       (frame-per-buffer :unsigned-long)
                                       (time-info :pointer)
                                       (status-flags pa-stream-callback-flags)
                                       (user-data :pointer))
  (declare (optimize (speed 3) (safety 0))
           (ignore input-buffer time-info status-flags user-data
                   frame-per-buffer))
  (progn ;; sb-sys:without-gcing しなくてもたいして変わらない
    (audio-loop)
    (write-master-buffer output-buffer)
    0))

(defun statistic-enter ()
  (let* ((now (get-internal-real-time))
         (delta (- now (.statistic-leave-time *audio*))))
    (setf (.statistic-enter-time *audio*) now)
    (incf (.statistic-total-interval-time *audio*) delta)
    (setf (.statistic-min-interval-time *audio*)
          (min (.statistic-min-interval-time *audio*) delta))
    (setf (.statistic-max-interval-time *audio*)
          (max (.statistic-max-interval-time *audio*) delta))))

(defun statistic-leave ()
  (let* ((now (get-internal-real-time))
         (delta (- now (.statistic-enter-time *audio*))))
    (setf (.statistic-leave-time *audio*) now)
    (incf (.statistic-total-process-time *audio*) delta)
    (setf (.statistic-min-process-time *audio*)
          (min (.statistic-min-process-time *audio*) delta))
    (setf (.statistic-max-process-time *audio*)
          (max (.statistic-max-process-time *audio*) delta))
    (when (<= (* (/ *sample-rate* *frames-per-buffer*) 10)
              (incf (.statistic-count *audio*)))
      (let ((cpu (* (/ (.statistic-total-process-time *audio*)
                       (+ (.statistic-total-process-time *audio*)
                          (.statistic-total-interval-time *audio*)))
                    100))
            (process-avg (/ (.statistic-total-process-time *audio*)
                            (.statistic-count *audio*)
                            internal-time-units-per-second))
            (process-min (/ (.statistic-min-process-time *audio*) internal-time-units-per-second))
            (process-max (/ (.statistic-max-process-time *audio*) internal-time-units-per-second))
            (interval-avg (/ (.statistic-total-interval-time *audio*)
                             (.statistic-count *audio*)
                             internal-time-units-per-second))
            (interval-min (/ (.statistic-min-interval-time *audio*) internal-time-units-per-second))
            (interval-max (/ (.statistic-max-interval-time *audio*) internal-time-units-per-second)))
        (format t "~&AUDIO CPU ~f% PROCESS ~fs ~fs ~fs INTERVAL ~fs ~fs ~fs FRAME ~,4fs "
                cpu
                process-avg process-min process-max
                interval-avg interval-min interval-max
                (/ *frames-per-buffer* *sample-rate*)))
      (setf (.statistic-total-process-time *audio*) 0)
      (setf (.statistic-min-process-time *audio*) most-positive-fixnum)
      (setf (.statistic-max-process-time *audio*) 0)
      (setf (.statistic-total-interval-time *audio*) 0)
      (setf (.statistic-min-interval-time *audio*) most-positive-fixnum)
      (setf (.statistic-max-interval-time *audio*) 0)
      (setf (.statistic-count *audio*) 0))))

;;; TODO DELETE
#+nil
(defmacro with-audio (&body body)
  `(progn
     (setf *audio* (make-instance 'audio-device))
     (portaudio:with-audio
       (cffi:with-foreign-objects ((handle :pointer))
         (unwind-protect
              (let* ((output-parameters (pa::make-stream-parameters)))
                (setf (pa:stream-parameters-channel-count output-parameters) (.output-channels *audio*)
                      (pa:stream-parameters-sample-format output-parameters) (.sample-format *audio*)
                      (pa::stream-parameters-suggested-latency output-parameters) 0.0d0) ;TODO
                (loop for i of-type fixnum below (pa:get-device-count)
                      for device-info = (pa:get-device-info i)
                      if (and
                          (equal (pa:host-api-info-name
                                  (pa:get-host-api-info (pa:device-info-host-api device-info)))
                                 (.device-api *audio*))
                          (equal (pa:device-info-name device-info) (.device-name *audio*)))
                        do (setf (pa::stream-parameters-device output-parameters) i)
                           (loop-finish))
                (setf (.stream *audio*)
                      (progn
                        (pa::raise-if-error
                         (pa::%open-stream
                          handle
                          nil
                          output-parameters
                          *sample-rate*
                          *frames-per-buffer*
                          0
                          (cffi:callback audio-callback)
                          (cffi:null-pointer)))
                        (make-instance
                         'pa:pa-stream
                         :handle (cffi:mem-ref handle :pointer)
                         :input-sample-format (.sample-format *audio*)
                         :input-channels (if (zerop (the fixnum (.input-channels *audio*)))
                                             nil
                                             (.input-channels *audio*))
                         :output-sample-format (.sample-format *audio*)
                         :output-channels (if (zerop (the fixnum (.output-channels *audio*)))
                                              nil
                                              (.output-channels *audio*))
                         :frames-per-buffer *frames-per-buffer*)))
                ,@body)
           (when (.stream *audio*)
             (stop-audio)
             (pa:close-stream (.stream *audio*))))))))



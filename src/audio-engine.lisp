(in-package :dgw)

;;(portaudio::print-devices)

(sb-ext:defglobal *app* nil)
(sb-ext:defglobal *audio* nil)
(defparameter *sample-rate* 48000.0)
(defparameter *frames-per-buffer* 1024)

(defclass audio-engine ()
  ((device-api
    :initarg :device-api
    :initform "ASIO"
    ;; :initform "MME"
    :accessor .device-api)
   (device-name
    :initarg :device-name
    :initform "Prism Sound USB Audio Class 2.0"
    ;; :initform "FL Studio ASIO"
    ;; :initform "Realtek Digital Output (Realtek"
    :accessor .device-name)
   (sample-rate
    :initarg :sample-rate
    :initform *sample-rate*
    :type single-float
    :accessor .sample-rate)
   (frames-per-buffer
    :initarg frames-per-buffer
    :initform *frames-per-buffer*
    :accessor .frames-per-buffer)
   (sample-format
    :initarg sample-format
    :initform :float
    :accessor .sample-format)
   (processing :initform nil :accessor .processing)
   (playing :initform nil :accessor .playing)
   (played :initform nil :accessor .played)
   (request-stop :initform nil :accessor .request-stop)
   (stream
    :initform nil
    :accessor .stream)
   (input-channels
    :initarg :input-channels
    :initform 0
    :type fixnum
    :accessor .input-channels)
   (output-channels
    :initarg :output-channels
    :initform 2
    :type fixnum
    :accessor .output-channels)
   (sequencer :initarg :sequencer :accessor .sequencer)
   (master-buffer :initform (list (make-array 1024 :element-type 'single-float :initial-element 0.0)
                                  (make-array 1024 :element-type 'single-float :initial-element 0.0))
                  :accessor .master-buffer)
   (process-thread :initform nil :accessor .process-thread)
   (process-thread-mailbox :accessor .process-thread-mailbox
                           :initform (sb-concurrency:make-mailbox))
   (audio-mailbox :accessor .audio-mailbox
                  :initform (sb-concurrency:make-mailbox))
   (statistic-enter-time :initform (get-internal-real-time)
                         :accessor .statistic-enter-time)
   (statistic-leave-time :initform (get-internal-real-time)
                         :accessor .statistic-leave-time)
   (statistic-count :initform 0 :accessor .statistic-count)
   (statistic-total-process-time :initform 0
                                 :accessor .statistic-total-process-time)
   (statistic-min-process-time :initform most-positive-fixnum
                               :accessor .statistic-min-process-time)
   (statistic-max-process-time :initform 0
                               :accessor .statistic-max-process-time)
   (statistic-total-interval-time :initform 0
                                  :accessor .statistic-total-interval-time)
   (statistic-min-interval-time :initform most-positive-fixnum
                                :accessor .statistic-min-interval-time)
   (statistic-max-interval-time :initform 0
                                :accessor .statistic-max-interval-time)))

(cffi:defbitfield (pa-stream-callback-flags :unsigned-long)
  (:input-underflow #x00000001)
  (:input-overflow #x00000002)
  (:output-underflow #x00000004)
  (:output-overflow #x00000008)
  (:priming-output #x00000010))

(cffi:defcstruct pa-stream-callback-time-info
  (input-buffer-adc-time pa::pa-time)
  (current-time pa::pa-time)
  (output-buffer-dac-time pa::pa-time))

(defun sec-per-line ()
  ;; (/ 60.0 (.bpm (.sequencer *audio*)) (.lpb (.sequencer *audio*)))
  (/ 60.0 128.0 4))

(defun sec-per-frame ()
  (/ 1.0 *sample-rate*))
(defun frames-per-line ()
  (/ (sec-per-line) (sec-per-frame)))

(defun start-audio ()
  (unless (.processing *audio*)
    (setf (.processing *audio*) t)
    (setf (.request-stop *audio*) nil)
    (pa:start-stream (.stream *audio*))))

(defun stop-audio ()
  (when (.processing *audio*)
    (setf (.processing *audio*) nil)
    (pa::stop-stream (.stream *audio*)))
  (swhen (.process-thread *audio*)
         (sb-thread:terminate-thread it)
         (setf it nil)))

(defun play-from-start ()
  (%play (make-play-position :line 0 :line-frame 0)))

(defun play-from-last ()
  (%play (.last-play-position (.sequencer *audio*))))

(defun play-from-current ()
  (let* ((sequencer (.sequencer *audio*)))
    (setf (.last-play-position sequencer) (.play-position sequencer))
    (%play (.play-position sequencer))))

(defun %play (play-position)
  (let* ((sequencer (.sequencer *audio*)))
    (setf (.play-position sequencer) play-position))
  (setf (.playing *audio*) t))

;; (defun stop ()
;;   (setf (.playing *audio*) nil))

(defun playing ()
  (.playing *audio*))

(defun played ()
  (.played *audio*))

(defun request-stop ()
  (setf (.request-stop *audio*) t))

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

(defun process-thread-loop ()
  ;; TODO (declare (optimize (speed 3) (safety 0)))
  (let ((thread-mailbox (.process-thread-mailbox *audio*))
        (audio-mailbox (.audio-mailbox *audio*)))
    (loop
      (sb-concurrency:receive-message thread-mailbox)
      (sb-sys:without-gcing
        (statistic-enter)
        (process *app*)

        ;; TODO かんぜんに暫定
        (let ((module (.module *app*)))
          (when (and module (.start-p module))
            (let ((in (.buffer-out module))
                  (out (.master-buffer *audio*)))
              (loop for channel below 2
                    do (loop for i below *frames-per-buffer*
                             for x = (nth channel out)
                             for y = (nth channel in)
                             do (setf (aref x i)
                                      (aref y i)))))))
        
        (statistic-leave))
      (sb-concurrency:send-message audio-mailbox t))))

(cffi:defcallback audio-callback :int ((input-buffer :pointer)
                                       (output-buffer :pointer)
                                       (frame-per-buffer :unsigned-long)
                                       (time-info (:pointer (:struct pa-stream-callback-time-info)))
                                       (status-flags pa-stream-callback-flags)
                                       (user-data :pointer))
  (declare (optimize (speed 3) (safety 0))
           (ignore input-buffer time-info status-flags user-data
                   frame-per-buffer))
  (let ((thread-mailbox (.process-thread-mailbox *audio*))
        (audio-mailbox (.audio-mailbox *audio*)))
    (sunless (.process-thread *audio*)
             (setf it (sb-thread:make-thread #'process-thread-loop
                                             :name "CoLiTrSynth process-thread-loop"))
             (sb-concurrency:send-message thread-mailbox t))
    (sb-concurrency:receive-message audio-mailbox)
    (write-master-buffer output-buffer)
    (sb-concurrency:send-message thread-mailbox t))
  0)

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

(defmacro with-audio (&body body)
  `(progn
     (setf *audio* (make-instance 'audio-engine))
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
                          (coerce *sample-rate* 'double-float)
                          (.frames-per-buffer *audio*)
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
                         :frames-per-buffer (.frames-per-buffer *audio*))))
                ,@body)
           (when (.stream *audio*)
             (stop-audio)
             (pa:close-stream (.stream *audio*))))))))



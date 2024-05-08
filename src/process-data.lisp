(in-package :dgw)

(defmethod p ((self sb:vst-process-data))
  (flet ((f (label audio-bus-buffer nbuses)
           (let ((ptr (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32 audio-bus-buffer)))
             (unless (autowrap:wrapper-null-p ptr)
               (loop for i below nbuses
                     do (format t "~a ~b" label (sb:vst-audio-bus-buffers.silence-flags audio-bus-buffer))
                        (loop for j below 10
                              with p = (autowrap:c-aref ptr i :pointer)
                              do (format t " ~2f" (autowrap:c-aref p j :float)))
                        (terpri))))))
    (f "in" (sb:vst-process-data.inputs* self) (sb:vst-process-data.num-inputs self))
    (f "out" (sb:vst-process-data.outputs* self) (sb:vst-process-data.num-outputs self))))

(defmethod prepare ((self sb:vst-process-data))
  ;; silence flag たてると process 後もたったままなのでバッファ初期化する
  (flet ((f (audio-bus-buffers nbuses)
           (loop for bus-index below nbuses
                 for bus = (c-ref audio-bus-buffers (:struct (sb:vst-audio-bus-buffers)) bus-index)
                 for channels = (c-ref bus (:struct (sb:vst-audio-bus-buffers)) :vst-audio-bus-buffers-channel-buffers32)
                 do (loop for channel-index below (c-ref bus (:struct (sb:vst-audio-bus-buffers)) :num-channels)
                          for channel = (c-ref channels :pointer channel-index)
                          do (loop for i below *frames-per-buffer*
                                   do (setf (c-ref channel :float i) .0))))))
    (f (sb:vst-process-data.inputs self) (sb:vst-process-data.num-inputs self))
    (f (sb:vst-process-data.outputs self) (sb:vst-process-data.num-outputs self)))
  (setf (sb:vst-process-data.inputs*.silence-flags self)
        0)
  (setf (sb:vst-process-data.outputs*.silence-flags self)
        0)

  ;; TODO midi event
  )

(defmethod inputs ((self sb:vst-process-data) bus-index)
  (cffi:inc-pointer (c-ref self (:struct (sb:vst-process-data))
                           :inputs)
                    (* bus-index (autowrap:sizeof '(:struct (sb:vst-audio-bus-buffers))))))

(defmethod inputs-channel ((self sb:vst-process-data) bus-index channel-index)
  (cffi:mem-aref (c-ref (inputs self bus-index)
                        (:struct (sb:vst-audio-bus-buffers))
                        :vst-audio-bus-buffers-channel-buffers32)
                 :pointer
                 channel-index))

(defmethod inputs-silence-flags ((self sb:vst-process-data) bus-index &optional channel-index)
  (let ((silence-flags (c-ref (inputs self bus-index)
              (:struct (sb:vst-audio-bus-buffers))
              :silence-flags)))
    (if channel-index
        (= (ldb (byte 1 channel-index) silence-flags) 1)
        silence-flags)))

(defmethod (setf inputs-silence-flags) (value (self sb:vst-process-data) bus-index &optional channel-index)
  (if channel-index
      (setf (ldb (byte 1 channel-index) #1=(c-ref (inputs self bus-index)
                                                  (:struct (sb:vst-audio-bus-buffers))
                                                  :silence-flags))
            (if value 1 0))
      (setf #1# value)))

(defmethod outputs ((self sb:vst-process-data) bus-index)
  (cffi:inc-pointer (c-ref self (:struct (sb:vst-process-data))
                           :outputs)
                    (* bus-index (autowrap:sizeof '(:struct (sb:vst-audio-bus-buffers))))))

(defmethod outputs-channel ((self sb:vst-process-data) bus-index channel-index)
  (cffi:mem-aref (c-ref (outputs self bus-index)
                        (:struct (sb:vst-audio-bus-buffers))
                        :vst-audio-bus-buffers-channel-buffers32)
                 :pointer
                 channel-index))

(defmethod outputs-silence-flags ((self sb:vst-process-data) bus-index &optional channel-index)
  (let ((silence-flags (c-ref (outputs self bus-index)
              (:struct (sb:vst-audio-bus-buffers))
              :silence-flags)))
    (if channel-index
        (= (ldb (byte 1 channel-index) silence-flags) 1)
        silence-flags)))

(defmethod (setf outputs-silence-flags) (value (self sb:vst-process-data) bus-index &optional channel-index)
  (if channel-index
      (setf (ldb (byte 1 channel-index) #1=(c-ref (outputs self bus-index)
                                                  (:struct (sb:vst-audio-bus-buffers))
                                                  :silence-flags))
            (if value 1 0))
      (setf #1# value)))

(defmethod terminate ((self sb:vst-process-data))
  (flet ((free (audio-bus-buffer nbuses)
           (let ((ptr (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32 audio-bus-buffer)))
             (unless (autowrap:wrapper-null-p ptr)
               (loop for i below nbuses
                     do (autowrap:free (autowrap:c-aref ptr i :pointer)))
               (autowrap:free ptr)
               (autowrap:free audio-bus-buffer)))))
    (free (sb:vst-process-data.inputs* self)
          (sb:vst-process-data.num-inputs self))
    (free (sb:vst-process-data.outputs* self)
          (sb:vst-process-data.num-outputs self))
    (autowrap:free self)))

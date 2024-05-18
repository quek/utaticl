(in-package :dgw)

(defmethod initialize-instance :after ((self audio-bus-buffers) &key (num-channels 2))
  (let* ((nbuses (.nbuses self))
         (ptr (autowrap:alloc-ptr '(:struct (sb:vst-audio-bus-buffers)) nbuses)))
    (setf (.ptr self) ptr)

    (loop for bus-index below nbuses
          for bus = (bus self bus-index)
          for buffer-ptr = (autowrap:alloc '(:pointer :float) num-channels)
          for buffers = (loop repeat num-channels
                              collect (autowrap:calloc :float (.frames-per-buffer *config*)))
          do (let ()
               (setf (c-ref bus (:struct (sb:vst-audio-bus-buffers)) :num-channels)
                     num-channels)
               (setf (c-ref bus (:struct (sb:vst-audio-bus-buffers)) :silence-flags)
                     0)
               (setf (c-ref bus (:struct (sb:vst-audio-bus-buffers)) :vst-audio-bus-buffers-channel-buffers32)
                     buffer-ptr)
               (loop for buffer in buffers
                     for i from 0
                     do (setf (cffi:mem-aref buffer-ptr '(:pointer :float) i)
                              buffer))))

    (sb-ext:finalize
     self
     (lambda ()
       (log:trace "audio-bus-buffers finalize free" ptr)
       (loop for bus below nbuses
             for p = (cffi:inc-pointer ptr (* (autowrap:sizeof '(:struct (sb:vst-audio-bus-buffers)))
                                              bus))
             for buffer-ptr = (c-ref p (:struct (sb:vst-audio-bus-buffers))
                                     :vst-audio-bus-buffers-channel-buffers32)
             do (loop for channel below num-channels
                      do (autowrap:free (cffi:mem-aref buffer-ptr '(:pointer :float) channel)))
                (autowrap:free buffer-ptr))
       (autowrap:free ptr)))))

(defmethod bus ((self audio-bus-buffers) bus-index)
  (cffi:inc-pointer (.ptr self) (* (autowrap:sizeof '(:struct (sb:vst-audio-bus-buffers)))
                                   bus-index)))

(defmethod buffer ((self audio-bus-buffers) bus-index channel-index)
  (let* ((bus (bus self bus-index))
         (buffer-ptr (c-ref bus (:struct (sb:vst-audio-bus-buffers))
                            :vst-audio-bus-buffers-channel-buffers32)))
    (cffi:mem-aref buffer-ptr '(:pointer :float) channel-index)))

(defmethod prepare ((self audio-bus-buffers))
  (loop for bus-index below (.nbuses self)
        for bus = (bus self bus-index)
        for num-channels = (c-ref bus (:struct (sb:vst-audio-bus-buffers))
                                  :num-channels)
        ;; silence flag たてると process 後もたったままなのでバッファ初期化する
        do (loop for channel-index below num-channels
                 for buffer = (buffer self bus-index channel-index)
                 do (loop for i below (.frames-per-buffer *config*)
                          do (setf (cffi:mem-aref buffer :float i) .0)))
           (setf (silence-flags self bus-index) 0)))

(defmethod silence-flags ((self audio-bus-buffers) bus-index &optional channel-index)
  (let* ((bus (bus self bus-index))
         (silence-flags (c-ref bus (:struct (sb:vst-audio-bus-buffers))
                               :silence-flags)))
    (if channel-index
        (= (ldb (byte 1 channel-index) silence-flags) 1)
        silence-flags)))

(defmethod (setf silence-flags) (value (self audio-bus-buffers) bus-index &optional channel-index)
  (let ((bus (bus self bus-index)))
    (if channel-index
        (setf (ldb (byte 1 channel-index) #1=(c-ref bus (:struct (sb:vst-audio-bus-buffers))
                                                    :silence-flags))
              (if value 1 0))
        (setf #1# value))))

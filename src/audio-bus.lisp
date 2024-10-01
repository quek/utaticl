(in-package :utaticl.core)

(defmethod initialize-instance :after ((self audio-bus) &key)
  (setf (.buffer self)
        (cffi:foreign-alloc :float
                            :count (* (.frames-per-buffer *config*) (.nchannels self))
                            :initial-element .0))
  (loop for i below (.nchannels self)
        do (const-set self i t)))

(defmethod buffer-at ((self audio-bus) channel)
  (cffi:inc-pointer (.buffer self)
                    (* (cffi:foreign-type-size :float)
                       (.frames-per-buffer *config*)
                       channel)))

(defmethod buffer-set-to-clap-audio-buffer ((self audio-bus) clap-audio-buffer)
  (loop for channel below (clap:clap-audio-buffer.channel-count clap-audio-buffer)
          do (setf (cffi:mem-aref (clap:clap-audio-buffer.data32 clap-audio-buffer)
                                  :pointer
                                  channel)
                   (buffer-at self channel))))

(defmethod const-set ((self audio-bus) channel bool)
  (setf (ldb (byte 1 channel) (.const self))
        (if bool 1 0)))

(defmethod const-get ((self audio-bus) channel)
  (plusp (ldb (byte 1 channel) (.const self))))

(defmethod prepare ((self audio-bus))
  (loop for channel below (.nchannels self)
        do (setf (cffi:mem-aref (buffer-at self channel) :float) .0)
           (const-set self channel t)))

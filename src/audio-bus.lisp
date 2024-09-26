(in-package :utaticl.core)

(defmethod initialize-instance :after ((audio-bus audio-bus) &key)
  (setf (.buffer audio-bus)
        (cffi:foreign-alloc :float
                            :count (* (.frames-per-buffer *config*) (.nchannels audio-bus))
                            :initial-element .0))
  (loop for i below (.nchannels audio-bus)
        do (const-set audio-bus i t)))

(defmethod const-set ((audio-bus audio-bus) channel bool)
  (setf (ldb (byte 1 channel) (.const audio-bus))
        (if bool 1 0)))

(defmethod const-get ((audio-bus audio-bus) channel)
  (plusp (ldb (byte 1 channel) (.const audio-bus))))

(defmethod prepare ((audio-bus audio-bus))
  (loop for buffer in (.buffer audio-bus)
        for i below (.nchannels audio-bus)
        do (setf (cffi:mem-aref buffer :float 0) .0)
        do (const-set audio-bus i t)))

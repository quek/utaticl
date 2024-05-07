(in-package :dgw)

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

(defmethod prepare ((self sb:vst-process-data))
  (setf (sb:vst-process-data.inputs*.silence-flags self)
        (1- (expt 2 (sb:vst-process-data.inputs*.num-channels self))))
  (setf (sb:vst-process-data.outputs*.silence-flags self)
        (1- (expt 2 (sb:vst-process-data.outputs*.num-channels self))))
  ;; TODO midi event
  )

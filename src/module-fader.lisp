(in-package :dgw)

(defmethod initialize-instance :after ((self module-fader) &key)
  (param-add self 'mute "Mute" .0d0)
  (param-add self 'pan "Pan" .5d0)
  (param-add self 'solo "Solo" .0d0)
  (param-add self 'volume "Volume" .8d0))

(defmethod process ((self module-fader))
  (let ((input (sb:vst-process-data.inputs*.vst-audio-bus-buffers-channel-buffers32
                *process-data*))
        (output (sb:vst-process-data.outputs*.vst-audio-bus-buffers-channel-buffers32
                 *process-data*)))
    (loop for channel below 2
          for input-channel = (autowrap:c-aref input channel :pointer)
          for output-channel = (autowrap:c-aref output channel :pointer)
          for input-silent-p = (= 1 (ldb (byte 1 0) (sb:vst-process-data.inputs*.silence-flags *process-data*)))
          for output-silent-p = (= 1 (ldb (byte 1 0) (sb:vst-process-data.outputs*.silence-flags *process-data*)))
          with volume-ratio = (.value (param self 'volume))
          if input-silent-p
            do (setf (ldb (byte 1 channel) (sb:vst-process-data.outputs*.silence-flags *process-data*)) 1)
          else
            do (loop for i below *frames-per-buffer*
                     do (setf (autowrap:c-aref output-channel i :float)
                              (coerce (* (if output-silent-p
                                             (autowrap:c-aref input-channel i :float)
                                             (+ (autowrap:c-aref output-channel i :float)
                                                (autowrap:c-aref input-channel i :float)))
                                         volume-ratio)
                                      'single-float)))))
  ;; (p *process-data*)
  (call-next-method))

(in-package :dgw)

(defmethod initialize-instance :after ((self module-gain) &key)
  (param-add self 'volume "Volume" .8d0))

(defmethod process ((self module-gain))
  (let ((input (sb:vst-process-data.inputs*.vst-audio-bus-buffers-channel-buffers32
                *process-data*))
        (silent-p (= 1 (ldb (byte 1 0) (sb:vst-process-data.inputs*.silence-flags *process-data*))))
        (output (sb:vst-process-data.outputs*.vst-audio-bus-buffers-channel-buffers32
                 *process-data*)))
    (if silent-p
        (setf (ldb (byte 1 0) (sb:vst-process-data.inputs*.silence-flags *process-data*)) 1)
        (progn
          (setf (ldb (byte 1 0) (sb:vst-process-data.inputs*.silence-flags *process-data*)) 0)
          (loop for channel below 2
                with volume-ratio = (.value (param self 'volume))
                do (loop for i below *frames-per-buffer*
                         for input-channel = (autowrap:c-aref input channel :pointer)
                         for output-channel = (autowrap:c-aref output channel :pointer)
                         do (setf (autowrap:c-aref output-channel i :float)
                                  (* (autowrap:c-aref input-channel i :float)
                                     volume-ratio)))))))
  (call-next-method))


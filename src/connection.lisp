(in-package :dgw)

(defmethod process ((self connection))
  (when (.start-p (.from self))
    (let ((input (sb:vst-process-data.inputs*.vst-audio-bus-buffers-channel-buffers32
                  (.from-process-data self)))
          (silent-p (= 1 (ldb (byte 1 0) (sb:vst-process-data.inputs*.silence-flags *process-data*))))
          (output (sb:vst-process-data.outputs*.vst-audio-bus-buffers-channel-buffers32
                   (.to-process-data self))))
      (if silent-p
          (setf (ldb (byte 1 0) (sb:vst-process-data.inputs*.silence-flags *process-data*)) 1)
          (progn
            (setf (ldb (byte 1 0) (sb:vst-process-data.inputs*.silence-flags *process-data*)) 0)
            (loop for channel below 2
                  do (loop for i below *frames-per-buffer*
                           for input-channel = (autowrap:c-aref input channel :pointer)
                           for output-channel = (autowrap:c-aref output channel :pointer)
                           ;; TODO 上書きではなく加算
                           do (setf (autowrap:c-aref output-channel i :float)
                                    (autowrap:c-aref input-channel i :float)))))))))

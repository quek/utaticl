(in-package :dgw)

(defmethod process ((self connection))
  (when (.start-p (.from self))
    (let* ((from-process-data (.from-process-data self))
           (from-channels (sb:vst-process-data.outputs*.vst-audio-bus-buffers-channel-buffers32
                           from-process-data))
           (to-process-data (.to-process-data self))
           (to-channels (sb:vst-process-data.inputs*.vst-audio-bus-buffers-channel-buffers32
                         to-process-data)))
      (loop for channel below 2
            for from-channel = (autowrap:c-aref from-channels channel :pointer)
            for to-channel = (autowrap:c-aref to-channels channel :pointer)
            for from-silent-p = (= 1 (ldb (byte 1 channel) (sb:vst-process-data.outputs*.silence-flags from-process-data)))
            for to-silent-p = (= 1 (ldb (byte 1 channel) (sb:vst-process-data.inputs*.silence-flags from-process-data)))
            if from-silent-p
              do (setf (ldb (byte 1 channel) (sb:vst-process-data.inputs*.silence-flags to-process-data)) 1)
            else
              do (loop for i below *frames-per-buffer*
                       do (setf (autowrap:c-aref to-channel i :float)
                                (if to-silent-p
                                    (autowrap:c-aref from-channel i :float)
                                    (+ (autowrap:c-aref from-channel i :float)
                                       (autowrap:c-aref to-channel i :float))))))

      ;; (p to-process-data)
      )))

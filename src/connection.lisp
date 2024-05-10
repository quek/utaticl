(in-package :dgw)

(defmethod process ((self connection))
  (when (.start-p (.from self))
    (let* ((from-process-data (.from-process-data self))
           (from-buses (.outputs from-process-data))
           (to-process-data (.to-process-data self))
           (to-buses (.inputs to-process-data)))
      (loop for channel-index below 2
            for from-channel = (buffer from-buses 0 channel-index)
            for to-channel = (buffer to-buses 0 channel-index)
            for from-silent-p = (silence-flags from-buses 0 channel-index)
            for to-silent-p = (silence-flags to-buses 0 channel-index)
            unless from-silent-p
              do (loop for i below *frames-per-buffer*
                       do (setf (cffi:mem-aref to-channel :float i)
                                (if to-silent-p
                                    (cffi:mem-aref from-channel :float i)
                                    (+ (cffi:mem-aref from-channel :float i)
                                       (cffi:mem-aref to-channel :float i)))))
                 (setf (silence-flags to-buses 0 channel-index) nil)))))

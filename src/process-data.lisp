(in-package :dgw)

(defmethod prepare ((self sb:vst-process-data))
  (setf (sb:vst-process-data.inputs*.silence-flags self)
        (1- (expt 2 (sb:vst-process-data.inputs*.num-channels self))))
  (setf (sb:vst-process-data.outputs*.silence-flags self)
        (1- (expt 2 (sb:vst-process-data.outputs*.num-channels self))))
  ;; TODO midi event
  )

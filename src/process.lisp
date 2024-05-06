(in-package :dgw)

(defgeneric process (self))

(defun swap-in-out (process-data)
  (psetf (sb:vst-process-data.inputs process-data)
         (sb:vst-process-data.outputs process-data)
         (sb:vst-process-data.outputs process-data)
         (sb:vst-process-data.inputs process-data)))


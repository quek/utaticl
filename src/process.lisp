(in-package :dgw)

(defvar *process-data*)

(defgeneric process (self))

(defun swap-in-out (process-data)
  (psetf (vst3-c-api:steinberg-vst-process-data.inputs process-data)
         (vst3-c-api:steinberg-vst-process-data.outputs process-data)
         (vst3-c-api:steinberg-vst-process-data.outputs process-data)
         (vst3-c-api:steinberg-vst-process-data.inputs process-data)))


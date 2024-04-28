(in-package :dgw)

(defmethod plugin-load ((self plugin-info-vst3))
  (let ((module (vst3-module-load (.path self))))
    (setf (.name module) (.name self))
    module))


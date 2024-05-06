(in-package :dgw)

(defmethod plugin-load ((self plugin-info-vst3))
  (let ((module (module-vst3-load (.path self))))
    (setf (.name module) (.name self))
    module))


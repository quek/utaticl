(in-package :utaticl.core)

(defmethod api ((plugin-info-vst3 plugin-info-vst3))
  "vst3")

(defmethod plugin-load ((plugin-info-vst3 plugin-info-vst3))
  (let ((module (module-vst3-load (.path plugin-info-vst3))))
    (setf (.name module) (.name plugin-info-vst3))
    module))



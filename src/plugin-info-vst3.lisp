(in-package :utaticl.core)

(defmethod api ((plugin-info-vst3 plugin-info-vst3))
  "vst3")

(defmethod plugin-load ((plugin-info-vst3 plugin-info-vst3))
  (make-instance 'module-vst3 :plugin-info plugin-info-vst3))



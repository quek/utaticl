(in-package :utaticl.core)

(defmethod initialize-instance :after ((param param) &key vst-parameter-info)
  (when vst-parameter-info
    (setf (.id param) (sb:vst-parameter-info.id vst-parameter-info))
    (setf (.name param) (sb:vst-parameter-info.title vst-parameter-info))
    (setf (.short-title param) (sb:vst-parameter-info.short-title vst-parameter-info))
    (setf (.units param) (sb:vst-parameter-info.units vst-parameter-info))
    (setf (.step-count param) (sb:vst-parameter-info.step-count vst-parameter-info))
    (setf (.default-normalized-value param)
          (sb:vst-parameter-info.default-normalized-value vst-parameter-info))
    (setf (.unit-id param) (sb:vst-parameter-info.unit-id vst-parameter-info))
    (setf (.flags param) (sb:vst-parameter-info.flags vst-parameter-info))))

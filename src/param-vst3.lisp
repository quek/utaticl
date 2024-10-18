(in-package :utaticl.core)

(defmethod initialize-instance :after ((self param-vst3) &key vst-parameter-info)
  (when vst-parameter-info
    (setf (.id self) (sb:vst-parameter-info.id vst-parameter-info))
    (setf (.name self)
          (vst3:from-string128 (sb:vst-parameter-info.title& vst-parameter-info)))
    (setf (.short-title self)
          (vst3:from-string128 (sb:vst-parameter-info.short-title& vst-parameter-info)))
    (setf (.units self)
          (vst3:from-string128 (sb:vst-parameter-info.units& vst-parameter-info)))
    (setf (.step-count self)
          (sb:vst-parameter-info.step-count vst-parameter-info))
    (setf (.default-value self)
          (sb:vst-parameter-info.default-normalized-value vst-parameter-info))
    (setf (.unit-id self) (sb:vst-parameter-info.unit-id vst-parameter-info))
    (setf (.flags self) (sb:vst-parameter-info.flags vst-parameter-info))))

(defmethod automate-p ((self param-vst3))
  (plusp (logand (.flags self)
                 sb:+vst-parameter-info-parameter-flags-k-can-automate+)))

(defmethod value-changed-by-host ((self param-vst3))
  (vst3-ffi::set-param-normalized (.controller (.module self))
                                  (.id self)
                                  (.value self))
  (param-change-add (.module self) (.id self) (.value self)))

(defmethod value-changed-by-processor ((self param-vst3))
  (break "value-changed-by-processor ~a" self)
  (vst3-ffi::set-param-normalized (.controller (.module self))
                                  (.id self)
                                  (.value self)))

(defmethod value-text ((self param-vst3) &optional (value (.value self)))
  (let ((text (autowrap:with-alloc (string128 'sb:vst-string128)
                (vst3-ffi::get-param-string-by-value (.controller (.module self))
                                                     (.id self)
                                                     value
                                                     string128)
                (vst3:from-string128 string128))))
    ;; Khz のプラグイン ナローノンブレイクスペース U+202F が入っていて化けるので
    (delete (code-char #x202F) text)))

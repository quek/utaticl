(in-package :dgw)

(defmethod .color :around ((self clip))
  (or (call-next-method)
      (.color (.seq self))))

(defmethod .name :around ((self clip))
  (or (call-next-method)
      (.name (.seq self))))

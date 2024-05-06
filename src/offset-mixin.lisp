(in-package :dgw)

(defmethod time-to-local-x ((self offset-mixin) time)
  (+ (call-next-method) (.offset-x self)))

(defmethod time-to-world-x ((self offset-mixin) time)
  (+ (call-next-method) (.offset-x self)))

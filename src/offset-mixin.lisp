(in-package :dgw)

(defmethod time-to-local-y ((self offset-mixin) time)
  (+ (call-next-method) (.offset-y self)))

(defmethod time-to-world-y ((self offset-mixin) time)
  (+ (call-next-method) (.offset-y self)))

(in-package :dgw)

(defmethod time-to-local-x ((self scroll-mixin) time)
  (call-next-method))

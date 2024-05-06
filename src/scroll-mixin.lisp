(in-package :dgw)

(defmethod time-to-local-x ((self scroll-mixin) time)
  (call-next-method))

(defmethod time-to-world-x ((self scroll-mixin) time)
  (- (call-next-method) (ig:get-scroll-x)))

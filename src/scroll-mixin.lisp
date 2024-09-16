(in-package :utaticl.core)

(defmethod time-to-local-y ((self scroll-mixin) time)
  (call-next-method))

(defmethod time-to-world-y ((self scroll-mixin) time)
  (- (call-next-method) (ig:get-scroll-y)))

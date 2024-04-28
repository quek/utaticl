(in-package :dgw)

(defmethod show ((self show-mixin))
  (setf (.show-p self) t))

(defmethod hide ((self show-mixin))
  (setf (.show-p self) nil))

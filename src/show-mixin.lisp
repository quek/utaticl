(in-package :utaticl.core)

(defmethod render :around ((show-mixin show-mixin))
  (when (.show-p show-mixin)
    (call-next-method)))

(defmethod hide ((self show-mixin))
  (setf (.show-p self) nil))

(defmethod show ((self show-mixin))
  (setf (.show-p self) t))

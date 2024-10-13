(in-package :utaticl.core)

(defmethod initialize-instance :after ((self seq-automation) &key)
  (when (string= "" (.name self))
    (setf (.name self) (name-new 'seq-automation "P"))))

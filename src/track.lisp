(in-package :dgw)

(defmethod module-add ((self track) module)
  (setf (.modules self) (append (.modules self) (list module))))

(defmethod render ((self track))
  (ig:push-id)
  (ig:button (.name self))
  (ig:pop-id))

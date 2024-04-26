(in-package :dgw)

(defmethod render ((self track))
  (ig:push-id)
  (ig:button (.name self))
  (ig:pop-id))

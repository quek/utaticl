(in-package :dgw)

(defmethod render ((self track) context)
  (ig:push-id-int (sxhash self))
  (ig:button (.name self))
  (ig:pop-id))

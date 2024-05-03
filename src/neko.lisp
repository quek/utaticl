(in-package :dgw)

(defmethod ig:push-id ((self neko))
  (ig:push-id (.neko-id self)))

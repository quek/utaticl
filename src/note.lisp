(in-package :dgw)

(defmethod .name ((self note))
  (midi-key-name (.key self)))

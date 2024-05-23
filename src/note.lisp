(in-package :dgw)

(defmethod move ((self note) time key)
  (setf (.time self) time)
  (setf (.key self) key))

(defmethod .name ((self note))
  (midi-key-name (.key self)))

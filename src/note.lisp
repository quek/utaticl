(in-package :dgw)

(defmethod in-p ((note note) (rect-piano-roll rect-piano-roll))
  (and (< (time-end note) (.time-start rect-piano-roll))
       (< (.time-end rect-piano-roll) (.time note))
       (<= (.key-start rect-piano-roll) (.key note) (.key-end rect-piano-roll))))

(defmethod move ((self note) time key)
  (setf (.time self) time)
  (setf (.key self) key))

(defmethod .name ((self note))
  (midi-key-name (.key self)))

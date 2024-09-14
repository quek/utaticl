(in-package :dgw)

(defmethod in-p ((note note) (rect-piano-roll rect-piano-roll))
  (and (< (time-end note) (.time-start rect-piano-roll))
       (< (.time-end rect-piano-roll) (.time note))
       (<= (.key-start rect-piano-roll) (.key note) (.key-end rect-piano-roll))))

(defmethod in-p ((note note) (rect cons))
  (destructuring-bind (key-start time-start key-end time-end) rect
   (and (< (time-end note) time-start)
        (< time-end (.time note))
        (<= key-start (.key note) key-end))))

(defmethod move ((self note) time key)
  (setf (.time self) time)
  (setf (.key self) key))

(defmethod .name ((self note))
  (midi-key-name (.key self)))

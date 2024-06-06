(in-package :dgw)

(defmethod time-end ((self time-thing))
  (+ (.time self) (.duration self)))

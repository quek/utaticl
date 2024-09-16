(in-package :utaticl.core)

(defmethod time-end ((self time-thing))
  (+ (.time self) (.duration self)))

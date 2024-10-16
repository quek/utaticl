(in-package :utaticl.core)

(defmethod dd-show ((self automation-point))
  (ig:text (format nil "~,2f@~,2f" (.value self) (.time self))))

(in-package :utaticl.core)

(defmethod dd-show ((self automation-point))
  (ig:text (format nil "~,2f@~,2f" (.value self) (.time self))))

(defmethod print-object ((self automation-point) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~,2f ~,2f" (.time self) (.value self))))

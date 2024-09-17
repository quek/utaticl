(in-package :utaticl.core)

(defmethod automate-p ((param param))
  t)

(defmethod value-changed-by-host ((param param)))

(defmethod value-text ((param param))
  (format nil "~,2f" (.value param)))


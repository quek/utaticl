(in-package :utaticl.core)

(defmethod automate-p ((param param))
  t)

(defmethod value-text ((param param) &key)
  (format nil "~,2f" (.value param)))

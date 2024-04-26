(in-package :dgw)

(defmethod render ((self app))
  (loop for project in (.projects self)
        do (render project)))

(defmethod terminate ((self app))
  (loop for project in (.projects self)
        do (terminate project)))

(defmethod process ((self app))
  (loop for project in (.projects self)
        do (process project)))

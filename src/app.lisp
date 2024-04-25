(in-package :dgw)

(defmethod render ((self app) context)
  (loop for project in (.projects self)
        do (render project context)))

(defmethod terminate ((self app))
  (loop for project in (.projects self)
        do (terminate project)))

(defmethod process ((self app))
  (loop for project in (.projects self)
        do (process project)))

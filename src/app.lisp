(in-package :dgw)

(defmethod cmd-run ((self app))
  (loop for project in (.projects self)
        do (cmd-run project)))

(defmethod render ((self app))
  (loop for project in (.projects self)
        do (render project)))

(defmethod terminate ((self app))
  (loop for project in (.projects self)
        do (terminate project)))

(defmethod process ((self app))
  (loop for project in (.projects self)
        do (process project)))

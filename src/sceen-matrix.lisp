(in-package :dgw)

(defmethod initialize-instance :after ((sceen-matrix sceen-matrix) &key)
  (sceen-add sceen-matrix (make-instance 'sceen)))

(defmethod sceen-add ((sceen-matrix sceen-matrix) (sceen sceen))
  (setf (.sceen-matrix sceen) sceen-matrix)
  (push (.sceens sceen-matrix) sceen))


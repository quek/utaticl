(in-package :dgw)

(defmethod clip-add ((sceen sceen) (clip clip) &key lane)
  (setf (.lane clip) lane)
  (setf (.sceen clip) sceen)
  (setf (gethash lane (.clips sceen)) clip))

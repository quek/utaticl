(in-package :dgw)

(defmethod clip-add ((sceen sceen) (clip clip) &key lane)
  (setf (.lane clip) lane)
  (setf (.sceen clip) sceen)
  (setf (gethash lane (.clips sceen)) clip))

(defmethod (setf .play-p) (value (sceen sceen))
  (unless value
    (loop for clip being each hash-value of (.clips sceen)
          do (setf (.play-p clip) nil))))

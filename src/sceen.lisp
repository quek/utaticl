(in-package :dgw)

(defmethod clip-add ((sceen sceen) (clip clip) &key lane)
  (setf (.lane clip) lane)
  (setf (.sceen clip) sceen)
  (setf (gethash lane (.clips sceen)) clip))

(defmethod (setf .play-p) (value (sceen sceen))
  (unless value
    (loop for clip being each hash-value of (.clips sceen)
          do (setf (.play-p clip) nil))))

(defmethod prepare-event ((sceen sceen) start end loop-p offset-samples)
  (loop for lane being the hash-key in (.clips sceen)
          using (hash-value clip)
        if (.play-p clip)
          do (let ((*process-data* (.process-data (.track lane))))
               (prepare-event clip start end loop-p offset-samples))))

(in-package :dgw)

(defmethod initialize-instance :after ((sceen sceen) &key)
  (when (string= "" (.name sceen))
    (setf (.name sceen) (name-new 'sceen "SCN"))))

(defmethod clip-add ((sceen sceen) (clip clip) &key lane)
  (let ((lane (if lane
                  (setf (.lane clip) lane)
                  (.lane clip))))
    (setf (.sceen clip) sceen)
    (setf (gethash lane (.clips sceen)) clip)))

(defmethod (setf .play-p) (value (sceen sceen))
  (unless value
    (loop for clip being each hash-value of (.clips sceen)
          do (setf (.play-p clip) nil))))

(defmethod prepare-event ((sceen sceen) start end loop-p offset-samples)
  (loop for lane being the hash-key in (.clips sceen)
          using (hash-value clip)
        if (or (.play-p clip)
               (.will-start clip))
          do (let* ((*process-data* (.process-data (.track lane)))
                    (duration (.duration clip))
                    (start (rem start duration))
                    (end (rem end duration)))
               (if (< start end)
                   (prepare-event clip start end loop-p offset-samples)
                   (progn
                     (prepare-event clip start duration t offset-samples)
                     (prepare-event clip .0d0 end nil
                                    (+ offset-samples
                                       (time-to-sample (.project sceen)
                                                       (- duration start)))))))))

(defmethod .project ((sceen sceen))
  (.project (.sceen-matrix sceen)))

(in-package :utaticl.core)

(defmethod initialize-instance :after ((sceen sceen) &key)
  (when (string= "" (.name sceen))
    (setf (.name sceen) (name-new 'sceen "SCN"))))

(defmethod clip-add ((sceen sceen) (clip clip) &key lane)
  (let ((lane (if lane
                  (setf (.lane clip) lane)
                  (.lane clip))))
    (setf (.sceen clip) sceen)
    (setf (gethash lane (.clips sceen)) clip)))

(defmethod clip-delete ((sceen sceen) clip)
  (let ((lane (.lane clip)))
    (setf (.lane clip) nil)
    (setf (.sceen clip) nil)
    (remhash lane (.clips sceen))))

(defmethod diff ((a sceen) (b sceen))
  (- (position a (.sceens (.sceen-matrix a)))
     (position b (.sceens (.sceen-matrix b)))))

(defmethod play ((sceen sceen))
  (map-lanes *project*
             (lambda (lane acc)
               (declare (ignore acc))
               (let ((clip (gethash lane (.clips sceen))))
                 (when clip
                   (enqueue (.sceen-matrix sceen) clip))))))

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

(defmethod relative-at ((self sceen) delta)
  (if (zerop delta)
      self
      (let* ((all (.sceens (.sceen-matrix self)))
             (pos (position self all)))
        (nth (min (max (+ pos delta) 0)
                  (1- (length all)))
             all))))

(defmethod stop ((sceen sceen))
  (loop for clip being the hash-value of (.clips sceen)
        do (when (.play-p clip)
             (setf (.will-stop clip) t)))
  (setf (.play-p sceen) nil))

(defmethod stop-immediate ((sceen sceen))
  (loop for clip being the hash-value of (.clips sceen)
        do (stop-immediate clip))
  (setf (.play-p sceen) nil))

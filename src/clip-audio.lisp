(in-package :dgw)

(defmethod initialize-instance :around ((clip-audio clip-audio) &key path)
  (if path
      (call-next-method clip-audio :path path :seq (make-instance 'seq-audio :path path))
      (call-next-method)))

(defmethod clip-add :after ((lane lane) (clip-audio clip-audio) &key)
  (update-duration clip-audio (.bpm (.project lane))))

(defmethod edit ((clip-audio clip-audio))
  ;; TODO
  (print "edit clip-audio."))

(defmethod prepare-event ((clip-audio clip-audio) start end loop-p offset-samples)
  ;; TODO
  )

(defmethod update-duration ((clip-audio clip-audio) bpm)
  (setf (.duration clip-audio) (update-duration (.seq clip-audio) bpm)))

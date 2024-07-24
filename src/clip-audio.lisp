(in-package :dgw)

(defmethod initialize-instance :after ((clip-audio clip-audio) &key path)
  (when path
    (setf (.seq clip-audio) (make-instance 'seq-audio :path path))))

(defmethod clip-add :after ((lane lane) (clip-audio clip-audio) &key)
  (update-duration clip-audio (.bpm (.project lane))))

(defmethod edit ((clip-audio clip-audio))
  ;; TODO
  (print "edit clip-audio."))

(defmethod prepare-event ((clip-audio clip-audio) start end loop-p offset-samples)
  (prepare-event (.seq clip-audio) start end loop-p offset-samples))

(defmethod update-duration ((clip-audio clip-audio) bpm)
  (setf (.duration clip-audio) (update-duration (.seq clip-audio) bpm)))

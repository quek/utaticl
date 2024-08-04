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
  (let ((clip-time (.time clip-audio)))
   (prepare-event (.seq clip-audio) (max (- start clip-time) .0d0) (- end clip-time) loop-p offset-samples)))

(defmethod render-in-arrangement ((clip-audio clip-audio) pos1 pos2 pos1-visible pos2-visible)
  (render-in-arrangement (.seq clip-audio) pos1 pos2 pos1-visible pos2-visible))

(defmethod stretch ((clip-audio clip-audio) duration)
  (let ((old-duration (.duration clip-audio)))
   (when (/= old-duration duration)
     (setf (.duration clip-audio) duration)
     (stretch (.seq clip-audio) duration))))

(defmethod update-duration ((clip-audio clip-audio) bpm)
  (setf (.duration clip-audio) (update-duration (.seq clip-audio) bpm)))

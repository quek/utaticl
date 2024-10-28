(in-package :utaticl.core)

(defmethod initialize-instance :after ((clip-audio clip-audio) &key path)
  (setf (.seq clip-audio) (make-instance 'seq-audio :path path)))

(defmethod clip-add :after ((lane lane) (clip-audio clip-audio) &key)
  (update-duration clip-audio (.bpm *project*)))

(defmethod edit ((self clip-audio) clips)
  (setf (.editor-audio *project*)
        (make-instance 'editor-audio :clip self)))

(defmethod prepare-event ((clip-audio clip-audio) start end loop-p offset-samples)
  (let ((clip-time (.time clip-audio)))
   (prepare-event (.seq clip-audio) (max (- start clip-time) .0d0) (- end clip-time) loop-p offset-samples)))

(defmethod draw ((clip-audio clip-audio) (arrangement arrangement)
                           &key pos size selection visible-pos visible-size)
  (call-next-method)
  (draw (.seq clip-audio) arrangement
                  :pos pos :size size :selection selection
                  :visible-pos visible-pos :visible-size visible-size))

(defmethod stretch ((clip-audio clip-audio) duration)
  (let ((old-duration (.duration clip-audio)))
   (when (/= old-duration duration)
     (setf (.duration clip-audio) duration)
     (stretch (.seq clip-audio) duration))))

(defmethod update-duration ((self clip-audio) bpm)
  (setf (.duration self) (update-duration (.seq self) bpm)))

(in-package :utaticl.core)

(defmethod initialize-instance :after ((self seq-audio) &key path)
  (let ((sample (make-instance 'sample :path path)))
    (push sample (.samples self))
    (when (string= "" (.name self))
      (setf (.name self) (.name sample)))))

(defmethod prepare-event ((self seq-audio) start end loop-p offset-samples)
  (loop for sample in (.samples self)
        do (prepare-event sample start end loop-p offset-samples)))

(defmethod stretch ((self seq-audio) duration)
  (loop for sample in (.samples self)
        do (stretch sample duration))
  (setf (.duration self) duration))

(defmethod update-duration ((self seq-audio) bpm)
  (loop for sample in (.samples self)
        do (setf (.duration self) (update-duration sample bpm)))
  (.duration self))

(defmethod render-in-arrangement ((self seq-audio) pos1 pos2 pos1-visible pos2-visible)
  (loop for sample in (.samples self)
        do (render-in-arrangement sample pos1 pos2 pos1-visible pos2-visible)))

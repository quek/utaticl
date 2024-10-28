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

(defmethod render-content ((self seq-audio) (arrangement arrangement)
                           &key pos size selection visible-pos visible-size)
  (loop for sample in (.samples self)
        do (render-content sample arrangement
                           :pos pos :size size :selection selection
                           :visible-pos visible-pos :visible-size visible-size)))

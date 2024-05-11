(in-package :dgw)

(defmethod .color :around ((self clip))
  (or (call-next-method)
      (.color (.seq self))))

(defmethod lane ((self clip))
  (labels ((g (lane)
             (if (member self (.clips lane))
                 lane
                 nil))
           (f (track)
             (or (loop for lane in (.lanes track)
                         thereis (g lane))
                 (loop for x in (.tracks track)
                         thereis (f x)))))
    (f (.master-track *project*))))

(defmethod .name :around ((self clip))
  (or (call-next-method)
      (.name (.seq self))))

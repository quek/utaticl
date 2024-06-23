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
    (f (.master-track (.project self)))))

(defmethod link-p ((self clip))
  (< 1 (length (.clips (.seq self)))))

(defmethod move ((self clip) time lane-to)
  (setf (.time self) time)
  (let ((lane-from (lane self)))
    (unless (eq lane-from lane-to)
      (clip-delete lane-from self)
      (clip-add lane-to self))))

(defmethod .name :around ((self clip))
  (or (call-next-method)
      (.name (.seq self))))

(defmethod .project ((self clip))
  (.project (.lane self)))

(defmethod (setf .seq) :after ((seq seq) (self clip))
  (push self (.clips seq)))

(defmethod terminate ((self clip))
  (let ((seq (.seq self)))
    (when seq
      (setf (.clips seq) (delete self (.clips seq))))))

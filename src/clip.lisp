(in-package :utaticl.core)

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

(defmethod render-content ((self clip) (arrangement arrangement)
                           &key pos size selection)
  (ig:with-id (self)
    (ig:set-cursor-pos-x (+ (.x pos) *text-margin*))
    (ig:text (format nil "~:[~;∞~]~a" (link-p self) (.name self)))
    (let ((color (color-selected (.color self) (include-p selection self)))
          (pos (local-to-world arrangement pos)))
      (ig:add-rect-filled *draw-list*
                          (@+ pos (@ 2.0 1.0))
                          (@+ pos size (@ -1.0 0.0))
                          color
                          :rounding 3.0))
    (ig:set-cursor-pos pos)
    (ig:invisible-button "##" size)))

(defmethod render-in ((self clip) (arrangement arrangement)
                      &key pos size selection)
  (call-next-method self arrangement :pos pos :size size :selection selection))

(defmethod (setf .seq) :after ((seq seq) (self clip))
  (push self (.clips seq)))

(defmethod stop ((clip clip))
  (setf (.will-stop clip) t))

(defmethod stop-immediate ((clip clip))
  (setf (.play-p clip) nil))

(defmethod terminate ((self clip) &key)
  (let ((seq (.seq self)))
    (when seq
      (setf (.clips seq) (delete self (.clips seq))))))

(in-package :utaticl.core)

(defmethod change-end ((self clip) delta)
  (incf (.duration self) delta))

(defmethod change-start ((self clip) delta)
  (incf (.time self) delta)
  (setf (.offset-start self)
        (abs (mod (+ (.offset-start self) delta)
                  (.duration (.seq self)))))
  (decf (.duration self) delta))

(defmethod .color :around ((self clip))
  (or (call-next-method)
      (.color (.seq self))))

(defmethod draw ((self clip) (arrangement arrangement)
                 &key pos size selection visible-pos visible-size)
  (declare (ignore visible-pos visible-size))
  (ig:with-id (self)
    (ig:set-cursor-pos-x (+ (.x pos) *text-margin*))
    (ig:text (format nil "~:[~;∞~]~a" (link-p self) (.name self)))
    (let* ((color (color-selected (.color self) (include-p selection self)))
           (pos (local-to-world arrangement pos))
           (pos1 (@+ pos (@ 2.0 1.0)))
           (pos2 (@+ pos size (@ -1.0 0.0))))
      (ig:add-rect-filled *draw-list* pos1 pos2 color :rounding 3.0)
      (loop for time from (- (.duration (.seq self)) (.offset-start self))
              below (.duration self) by (.duration (.seq self))
            for y = (coerce (* time (/ (.y size) (.duration self))) 'single-float)
            for pos1 = (@+ pos (@ 0.0 y))
            for pos2 = (@+ pos1 (@ (.x size) 0.0))
            if (plusp y)
              do (ig:add-line *draw-list* pos1 pos2 (.color-line *theme*))))
    (ig:set-cursor-pos pos)
    (ig:invisible-button "##" size)))

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

(defmethod prepare-event :around ((self clip) start end loop-p offset-samples)
  "clip loop"
  (let* ((time-clip (.time self))
         (offset-start (.offset-start self))
         (duration-seq (.duration (.seq self)))
         (offset (+ time-clip
                    (- offset-start)
                    (* (floor (/ (+ start (- time-clip) offset-start)
                                 duration-seq))
                       duration-seq))))
    (call-next-method self (- start offset) (- end offset) loop-p offset-samples)))

(defmethod .project ((self clip))
  (.project (.lane self)))

(defmethod render-in ((self clip) (arrangement arrangement)
                      &key pos size selection if-at-mouse visible-pos visible-size)
  (call-next-method self arrangement
                    :pos pos :size size :selection selection
                    :if-at-mouse if-at-mouse
                    :visible-pos visible-pos
                    :visible-size visible-size))

(defmethod (setf .seq) :after ((seq seq) (self clip))
  (push self (.clips seq)))

(defmethod stop ((clip clip))
  (setf (.will-stop clip) t))

(defmethod stop-immediate ((clip clip))
  (setf (.play-p clip) nil))

(defmethod stretch-end ((self clip) delta)
  (incf (.duration self) delta))

(defmethod stretch-start ((self clip) delta)
  (incf (.time self) delta))

(defmethod terminate ((self clip) &key)
  (let ((seq (.seq self)))
    (when seq
      (setf (.clips seq) (delete self (.clips seq))))))

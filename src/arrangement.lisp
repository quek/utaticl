(in-package :dgw)

(defmethod handle-mouse ((self arrangement))
  (let* ((mouse-pos (ig:get-mouse-pos)))
    (cond ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
           (multiple-value-bind (time lane) (world-pos-to-time-lane self mouse-pos)
             (cmd-add *project* 'cmd-clip-add :time time :lane-id (.neko-id lane)))))))

(defmethod max-bar ((self arrangement))
  ;; TODO
  8)

(defmethod lane-height ((self arrangement) (lane lane))
  (sif (gethash lane (.lane-height-map self))
       it
       (setf it (+ (.default-lane-height self)
                   (* (c-ref (ig:get-style) ig:im-gui-style :item-spacing :y)
                      2)))))

(defmethod world-pos-to-time-lane ((self arrangement) pos)
  (let* ((time (world-x-to-time self (.x pos)))
         (lane (world-y-to-lane self (.y pos))))
    (values time lane)))

(defmethod world-x-to-time ((self arrangement) x)
  (+ (/ (- x (.x (ig:get-window-pos)) (.track-width self))
        (.zoom-x self))
     (ig:get-scroll-x)))

(defmethod world-y-to-lane ((self arrangement) y)
  (let ((local-y (+ (- y (.y (ig:get-window-pos)) (.time-ruler-height self))
                    (ig:get-scroll-y))))
    (labels ((f (track height)
               (or (loop for lane in (.lanes track)
                           thereis (and (< local-y (incf height (lane-height self lane))) lane))
                   (loop for track in (.tracks track)
                           thereis (f track height)))))
      (f (.master-track *project*) 0))))

(defmethod render ((self arrangement))
  (when (ig:begin "##arrangement" (cffi:null-pointer) ig:+im-gui-window-flags-no-scrollbar+)
    (when (ig:begin-child "##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

      (render-time-ruler self)

      (let ((pos (ig:get-cursor-pos))
            (scroll-y (ig:get-scroll-y))
            (window-pos (ig:get-window-pos)))

        (ig:set-cursor-pos (@+ pos (@ (ig:get-scroll-x) (- scroll-y))))

        (ig:with-clip-rect ((@+ window-pos (@ .0 (- (.y pos) scroll-y 3)))
                            (@+ window-pos (ig:get-window-size)))
          (ig:begin-group)
          (render-track self (.master-track *project*))
          (ig:end-group)))

      (draw-horizontal-line (ig:get-cursor-pos))

      (let ((pos (ig:get-cursor-pos)))
        (ig:set-cursor-pos (@+ pos (@ (ig:get-scroll-x) 0.0)))
        (ig:set-next-item-shortcut (logior ig:+im-gui-mod-ctrl+ ig:+im-gui-key-t+))
        (when (ig:button "+" (@ (.track-width self) 0.0))
          (cmd-add *project* 'cmd-track-add)))

      (render-clip self (.master-track *project*) nil nil (.time-ruler-height self))

      (handle-mouse self))

    (ig:end-child)
    (shortcut-common))
  (ig:end))

(defmethod render-clip ((self arrangement) (track track) (lane null) (clip null) y)
  (loop for lane in (.lanes track)
        do (setf y (render-clip self track lane nil y)))
  (loop for track in (.tracks track)
        do (setf y (render-clip self track nil nil y)))
  y)

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip null) y)
  (loop for clip in (.clips lane)
        do (render-clip self track lane clip y))
  ;; この 4.0 は意味わかんない
  (+ y (lane-height self lane) 4.0))

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip clip) y)
  (let* ((draw-list (ig:get-window-draw-list))
         (x (time-to-local-x self (.time clip)))
         (scroll-pos (@ (ig:get-scroll-x) (ig:get-scroll-y)))
         (pos1 (@ (+ x (.track-width self)) y))
         (pos2 (@ (time-to-local-x self (+ (.time clip) (.duration clip)))
                  (+ y (lane-height self lane))))
         (window-pos (ig:get-window-pos)))
    (ig:set-cursor-pos pos1)
    (ig:text (format nil "  ~a" (.name clip)))

    (ig:add-rect-filled draw-list (@+ pos1 window-pos (@- scroll-pos))
                        (@+ pos2 window-pos (@- scroll-pos)) (.color clip)
                        :rounding 3.0)))

(defmethod render-time-ruler ((self arrangement))
  (let* ((draw-list (ig:get-window-draw-list))
         (max-bar (max-bar self))
         (window-pos (ig:get-window-pos))
         (window-size (ig:get-window-size))
         (scroll-x (ig:get-scroll-x))
         (scroll-y (ig:get-scroll-y)))
    (loop for bar from 0 to max-bar
          for x = (+ (* bar 4 (.zoom-x self))
                     (.track-width self))
          for cursor-pos = (list x scroll-y)
          do (when (<= (+ (.track-width self) scroll-x) (.x cursor-pos))
               (ig:set-cursor-pos cursor-pos)
               (ig:text (format nil " ~d" (1+ bar)))
               (let* ((p1 (@+ cursor-pos window-pos (@ (- scroll-x) (- scroll-y))))
                      (p2 (@+ p1 (@ 0.0 (.y window-size)))))
                 (when (<= (+ (.track-width self) (.x window-pos)) (.x p1))
                   (ig:add-line draw-list p1 p2 (.color-line *theme*))))))))

(defmethod render-track ((self arrangement) track)
  (ig:with-id (track)
    (draw-horizontal-line (@- (ig:get-cursor-pos) (@ (ig:get-scroll-x) 0.0)))
    (let ((pos (ig:get-cursor-pos)))
      (ig:text (format nil "  ~a" (.name track)))
      (ig:set-cursor-pos pos)
      (let ((color (color+ (.color track)
                           (if (.select-p track)
                               (color #x30 #x30 #x30 #x00)
                               (color 0 0 0 0)))))
        (ig:with-button-color (color)
          (when (ig:button "##_" (@ (.track-width self)
                                    (lane-height self (car (.lanes track)))))
            (let ((io (ig:get-io)))
              (when (zerop (c-ref io ig:im-gui-io :key-ctrl))
                (unselect-all-tracks *project*))
              (setf (.select-p track) t))))))
    (loop for x in (.tracks track)
          do (render-track self x))))

(defmethod time-to-local-x ((self arrangement) time)
  (coerce (* time (.zoom-x self)) 'single-float))

(defmethod .track-height ((self arrangement) track)
  ;; TODO
  60.0)

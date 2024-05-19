(in-package :dgw)

(defmethod handle-click ((self arrangement) clip-at-mouse)
  (if clip-at-mouse
      (if (key-ctrl-p)
          (if (member clip-at-mouse (.clips-selected self))
              (setf (.clips-selected self)
                    (print (delete clip-at-mouse (.clips-selected self))))
              (push clip-at-mouse (.clips-selected self)))
          (setf (.clips-selected self) (list clip-at-mouse)))
      (setf (.clips-selected self) nil)))

(defmethod handle-double-click ((self arrangement) clip-at-mouse)
  (if clip-at-mouse
      (cmd-add *project* 'cmd-clip-delete :clip-id (.neko-id clip-at-mouse))
      (multiple-value-bind (time lane) (world-pos-to-time-lane self (ig:get-mouse-pos))
        (setf time (time-grid-applied self time :floor))
        (when (and (not (minusp time)) lane)
          (cmd-add *project* 'cmd-clip-add
                   :time time :lane-id (.neko-id lane)
                   :execute-after (lambda (cmd)
                                    (edit (find-neko (.clip-id cmd)))))))))

(defmethod handle-dragging ((self arrangement))
  (if (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
      ;; TODO 移動 or 複製 command
      (progn
        (loop for clip in (.clips-dragging self)
              for lane = (gethash clip (.clip-lane-map self))
              do (clip-delete lane clip))
        (setf (.clips-dragging self) nil))
      ;; ドラッグ中の表示
      (multiple-value-bind (time lane) (world-pos-to-time-lane self (ig:get-mouse-pos))
        (let ((delta-time (- time (.time (.clip-target self))))
              (delta-lane (diff lane (gethash (.clip-target self) (.clip-lane-map self)))))
          (loop for dragging in (.clips-dragging self)
                for selected in (.clips-selected self)
                for time = (+ (.time selected) delta-time)
                for lane = (relative-at (gethash selected (.clip-lane-map self)) delta-lane)
                do (move dragging time lane))))))

(defmethod handle-drag-start ((self arrangement) clip-at-mouse)
  (if (and (.clips-selected self) clip-at-mouse)
      (progn
        ;; ノートの移動 or 長さ変更
        (setf (.clip-target self) clip-at-mouse)
        (setf (.clips-dragging self) (mapcar #'copy (.clips-selected self)))
        (loop for clip in (.clips-dragging self)
              for src-clip in (.clips-selected self)
              for lane = (gethash src-clip (.clip-lane-map self))
              do (clip-add lane clip)))
      ;; 範囲選択
      (setf (.range-selecting-p self) t)))

(defmethod handle-mouse ((self arrangement))
  (let* ((io (ig:get-io))
         (clip-at-mouse (.clip-at-mouse self)))
    (cond ((.clips-dragging self)
           (handle-dragging self))
          ((.range-selecting-p self)
           (handle-range-selecting self))
          ((ig:is-mouse-dragging ig:+im-gui-mouse-button-left+ 0.1)
           (handle-drag-start self clip-at-mouse))
          ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
           (handle-double-click self clip-at-mouse))
          ((ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
           (handle-click self clip-at-mouse))
          ((ig:is-mouse-released ig:+im-gui-mouse-button-left+)
           (handle-mouse-released self clip-at-mouse)))
    (zoom-x-update self io)))

(defmethod handle-mouse-released ((self arrangement) clip-at-mouse)
)

(defmethod lane-height ((self arrangement) (lane lane))
  (sif (gethash lane (.lane-height-map self))
       it
       (setf it (+ (.default-lane-height self)
                   (* (c-ref (ig:get-style) ig:im-gui-style :item-spacing :y)
                      2)))))

(defmethod handle-range-selecting ((self arrangement))
  ;; TODO
  (when (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
    (setf (.range-selecting-p self) nil)))

(defmethod render ((self arrangement))
  (setf (.clip-at-mouse self) nil)
  (clrhash (.clip-lane-map self))

  (ig:with-begin ("##arrangement" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (render-grid self)
    (ig:with-begin-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

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
        (when (ig:button "+" (@ (.offset-x self) 0.0))
          (cmd-add *project* 'cmd-track-add
                   :track-id-parent (.neko-id (.master-track *project*)))))

      (render-clip self (.master-track *project*) nil nil (.time-ruler-height self))

      (handle-mouse self))
    (shortcut-common)))

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
  (setf (gethash clip (.clip-lane-map self)) lane)
  (let* ((draw-list (ig:get-window-draw-list))
         (x1 (time-to-local-x self (.time clip)))
         (x2 (time-to-local-x self (+ (.time clip) (.duration clip))))
         (scroll-pos (@ (ig:get-scroll-x) (ig:get-scroll-y)))
         (pos1 (@ x1 y))
         (pos2 (@ x2 (+ y (lane-height self lane))))
         (window-pos (ig:get-window-pos))
         (mouse-pos (ig:get-mouse-pos)))
    (ig:set-cursor-pos pos1)
    (ig:text (format nil "  ~a" (.name clip)))

    (let ((pos1 (@+ pos1 window-pos (@- scroll-pos)))
          (pos2 (@+ pos2 window-pos (@- scroll-pos)))
          (color (color-selected (.color clip) (member clip (.clips-selected self)))))
      (ig:add-rect-filled draw-list
                          pos1
                          (@+ pos2 (@ .0 -1.0))
                          color
                          :rounding 3.0)
      (when (contain-p mouse-pos pos1 pos2)
        (setf (.lane-at-mouse self) lane)
        (setf (.clip-at-mouse self) clip)))))

(defmethod render-track ((self arrangement) track)
  (ig:with-id (track)
    (draw-horizontal-line (@- (ig:get-cursor-pos) (@ (ig:get-scroll-x) 0.0)))
    (let ((pos (ig:get-cursor-pos)))
      (ig:text (format nil "  ~a" (.name track)))
      (ig:set-cursor-pos pos)
      (let ((color (color-selected (.color track) (.select-p track))))
        (ig:with-button-color (color)
          (when (ig:button "##_" (@ (.offset-x self)
                                    (lane-height self (car (.lanes track)))))
            (unless (key-ctrl-p)
              (unselect-all-tracks *project*))
            (setf (.select-p track) t)))))
    (loop for x in (.tracks track)
          do (render-track self x))))

(defmethod .track-height ((self arrangement) track)
  ;; TODO
  60.0)

(defmethod world-pos-to-time-lane ((self arrangement) pos)
  (let* ((time (world-x-to-time self (.x pos)))
         (lane (world-y-to-lane self (.y pos))))
    (values time lane)))

(defmethod world-x-to-time ((self arrangement) x)
  (+ (/ (- x (.x (ig:get-window-pos)) (.offset-x self))
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

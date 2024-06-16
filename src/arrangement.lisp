(in-package :dgw)

(defmethod handle-click ((self arrangement))
  (if (.clip-at-mouse self)
      (when (and (not (member (.clip-at-mouse self) (.clips-selected self)))
                 (not (key-ctrl-p)))
        (setf (.clips-selected self) (list (.clip-at-mouse self))))
      (setf (.clips-selected self) nil)))

(defmethod handle-double-click ((self arrangement))
  (if (.clip-at-mouse self)
      (cmd-add *project* 'cmd-clip-delete :clip-id (.neko-id (.clip-at-mouse self)))
      (multiple-value-bind (time lane) (world-pos-to-time-lane self (ig:get-mouse-pos))
        (setf time (time-grid-applied self time #'floor))
        (when (and (not (minusp time)) lane)
          (cmd-add *project* 'cmd-clip-add
                   :time time :lane-id (.neko-id lane)
                   :execute-after (lambda (cmd)
                                    (edit (find-neko (.clip-id cmd)))))))))

(defmethod handle-dragging ((self arrangement))
  (if (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
      ;; ドラッグの終了
      (progn
        (if (key-ctrl-p)
            ;; 複製
            (cmd-add *project* 'cmd-clips-d&d-copy
                     :clips (.clips-dragging self)
                     :lane-ids (loop for clip in (.clips-dragging self)
                                     collect (.neko-id (gethash clip (.clip-lane-map self)))))
            ;; 移動
            (progn
              (cmd-add *project* 'cmd-clips-d&d-move
                       :clips (.clips-selected self)
                       :lane-ids (loop for clip in (.clips-selected self)
                                       collect (.neko-id (gethash clip (.clip-lane-map self))))
                       :times-to (mapcar #'.time (.clips-dragging self))
                       :lane-ids-to (mapcar #'(lambda (clip)
                                                (.neko-id (gethash clip (.clip-lane-map self))))
                                            (.clips-dragging self)))
              (loop for clip in (.clips-dragging self)
                    for lane = (gethash clip (.clip-lane-map self))
                    do (clip-delete lane clip))))
        (setf (.clips-dragging self) nil))
      ;; ドラッグ中の表示
      (multiple-value-bind (time lane)
          (world-pos-to-time-lane self
                                  (@- (ig:get-mouse-pos)
                                      (@ .0 (.clip-drag-offset self))))
        (setf time (max (time-grid-applied self time #'floor) .0d0))
        (let ((delta-time (- time (.time (.clip-target self))))
              (delta-lane (diff lane (gethash (.clip-target self) (.clip-lane-map self)))))
          (loop for dragging in (.clips-dragging self)
                for selected in (.clips-selected self)
                for time = (+ (.time selected) delta-time)
                for lane-selected = (gethash selected (.clip-lane-map self))
                for lane = (relative-at lane-selected delta-lane)
                do (move dragging time lane))))))

(defmethod handle-drag-start ((self arrangement))
  (if (and (.clips-selected self) (.clip-at-mouse self))
      (progn
        ;; ノートの移動 or 長さ変更
        (setf (.clip-target self) (.clip-at-mouse self))
        (setf (.clip-drag-offset self) (- (.y (ig:get-mouse-pos))
                                          (time-to-world-y self (.time (.clip-at-mouse self)))))
        (setf (.clips-dragging self) (mapcar #'copy (.clips-selected self)))
        (loop for clip in (.clips-dragging self)
              for src-clip in (.clips-selected self)
              for lane = (gethash src-clip (.clip-lane-map self))
              do (clip-add lane clip)))
      ;; 範囲選択
      (setf (.range-selecting-p self) t)))

(defmethod handle-mouse ((self arrangement))
  (let* ((io (ig:get-io)))
    (cond ((.clips-dragging self)
           (handle-dragging self))
          ((.range-selecting-p self)
           (handle-range-selecting self))
          ((ig:is-mouse-dragging ig:+im-gui-mouse-button-left+ 0.1)
           (handle-drag-start self))
          ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
           (handle-double-click self))
          ((ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
           (handle-click self))
          ((ig:is-mouse-released ig:+im-gui-mouse-button-left+)
           (handle-mouse-released self)))
    (zoom-y-update self io)))

(defmethod handle-mouse-released ((self arrangement))
  (if (.clip-at-mouse self)
      (if (member (.clip-at-mouse self) (.clips-selected self))
          (if (key-ctrl-p)
              (setf (.clips-selected self)
                    (delete (.clip-at-mouse self) (.clips-selected self)))
              (setf (.clips-selected self) (list (.clip-at-mouse self))))
          (if (key-ctrl-p)
              (push (.clip-at-mouse self) (.clips-selected self))))))

(defmethod lane-width ((self arrangement) (lane lane))
  (sif (gethash lane (.lane-width-map self))
       it
       (setf it (+ (.default-lane-width self)
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

      (let ((pos (@ (.time-ruler-width self) .0))
            (scroll-x (ig:get-scroll-x))
            (scroll-y (ig:get-scroll-y))
            (window-pos (ig:get-window-pos)))

        (ig:set-cursor-pos (@+ pos (@ (- scroll-x) .0)))

        (ig:with-clip-rect ((@+ window-pos (@ (- (.x pos) scroll-x 3.0) .0))
                            (@+ window-pos (ig:get-window-size)))
          (render-track self (.master-track *project*))))

      (draw-horizontal-line (ig:get-cursor-pos))

      (let ((pos (ig:get-cursor-pos)))
        (ig:set-cursor-pos (@+ pos (@ (ig:get-scroll-x) 0.0)))
        (ig:set-next-item-shortcut (logior ig:+im-gui-mod-ctrl+ ig:+im-gui-key-t+))
        (when (ig:button "+" (@ 0.0 (.offset-y self)))
          (cmd-add *project* 'cmd-track-add
                   :track-id-parent (.neko-id (.master-track *project*))
                   :execute-after (lambda (cmd)
                                    (let ((track (find-neko (.track-id-new cmd))))
                                      (unselect-all-tracks *project*)
                                      (setf (.select-p track) t))))))

      (render-clip self (.master-track *project*) nil nil (.time-ruler-width self))

      (handle-mouse self))
    (shortcut-common)))

(defmethod render-clip ((self arrangement) (track track) (lane null) (clip null) x)
  (loop for lane in (.lanes track)
        do (setf x (render-clip self track lane nil x)))
  (loop for track in (.tracks track)
        do (setf x (render-clip self track nil nil x)))
  x)

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip null) x)
  (loop for clip in (.clips lane)
        do (render-clip self track lane clip x))
  ;; この 8.0 は ItemSpacing じゃないかな
  (+ x (lane-width self lane) 8.0))

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip clip) x)
  (setf (gethash clip (.clip-lane-map self)) lane)
  (let* ((draw-list (ig:get-window-draw-list))
         (y1 (time-to-local-y self (.time clip)))
         (y2 (time-to-local-y self (+ (.time clip) (.duration clip))))
         (scroll-pos (@ (ig:get-scroll-x) (ig:get-scroll-y)))
         (pos1 (@ x y1))
         (pos2 (@ (+ x (lane-width self lane)) y2))
         (window-pos (ig:get-window-pos))
         (mouse-pos (ig:get-mouse-pos)))
    (ig:set-cursor-pos pos1)
    (ig:text (format nil "  ~a" (.name clip)))

    (let ((pos1 (@+ pos1 window-pos (@- scroll-pos)))
          (pos2 (@+ pos2 window-pos (@- scroll-pos)))
          (color (color-selected (.color clip) (member clip (.clips-selected self)))))
      (ig:add-rect-filled draw-list
                          pos1
                          pos2
                          color
                          :rounding 3.0)
      (when (contain-p mouse-pos pos1 pos2)
        (setf (.lane-at-mouse self) lane)
        (setf (.clip-at-mouse self) clip)))))

(defmethod render-track ((self arrangement) track)
  (ig:with-id (track)
    (draw-vertical-line (@- (ig:get-cursor-pos) (@ 0.0 (ig:get-scroll-y))))
    (let ((pos (ig:get-cursor-pos)))
      (ig:text (format nil "  ~a" (.name track)))
      (ig:set-cursor-pos pos)
      (let ((color (color-selected (.color track) (.select-p track))))
        (ig:with-button-color (color)
          (when (ig:button "##_" (@ (lane-width self (car (.lanes track)))
                                    (.offset-y self)))
            (unless (key-ctrl-p)
              (unselect-all-tracks *project*))
            (setf (.select-p track) t))))
      (ig:same-line))
    (loop for x in (.tracks track)
          do (render-track self x))))

(defmethod .track-height ((self arrangement) track)
  ;; TODO
  60.0)

(defmethod world-pos-to-time-lane ((self arrangement) pos)
  (let* ((time (world-y-to-time self (.y pos)))
         (lane (world-x-to-lane self (.x pos))))
    (values time lane)))

(defmethod world-y-to-time ((self arrangement) y)
  (+ (/ (- y (.y (ig:get-window-pos)) (.offset-y self))
        (.zoom-y self))
     (ig:get-scroll-y)))

(defmethod world-x-to-lane ((self arrangement) x)
  (let ((local-x (+ (- x (.x (ig:get-window-pos)) (.time-ruler-width self))
                    (ig:get-scroll-x)))
        (last-lane nil))
    (labels ((f (track width)
               (or (loop for lane in (.lanes track)
                         do (setf last-lane lane)
                           thereis (and (< local-x (incf width (lane-width self lane))) lane))
                   (loop for track in (.tracks track)
                           thereis (f track width)))))
      (or (f (.master-track *project*) 0)
          last-lane))))

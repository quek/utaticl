(in-package :dgw)

(defmethod compute-offset-y ((self arrangement))
  (labels ((f (track group-level)
             (if (and (.tracks-show-p track) (.tracks track))
                 (apply #'max (mapcar (lambda (x) (f x (1+ group-level)))
                                      (.tracks track)))
                 group-level)))
    (setf (.offset-y self)
          (+ 20.0
             (* (.offset-group self)
                (f (.master-track (.project self)) 1))))))

(defmethod handle-click ((self arrangement))
  (if (.clip-at-mouse self)
      (when (and (not (member (.clip-at-mouse self) (.clips-selected self)))
                 (not (key-ctrl-p)))
        (setf (.clips-selected self) (list (.clip-at-mouse self))))
      (setf (.clips-selected self) nil)))

(defmethod handle-double-click ((self arrangement))
  (if (.clip-at-mouse self)
      (edit (.clip-at-mouse self))
      (multiple-value-bind (time lane) (world-pos-to-time-lane self (ig:get-mouse-pos))
        (setf time (time-grid-applied self time #'floor))
        (when (and (not (minusp time)) lane)
          (cmd-add (.project self) 'cmd-clip-add
                   :time time :lane-id (.neko-id lane)
                   :execute-after (lambda (cmd)
                                    (edit (find-neko (.clip-id cmd)))))))))

(defmethod handle-dragging ((self arrangement))
  (if (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
      ;; ドラッグの終了
      (progn
        (if (key-ctrl-p)
            ;; 複製
            (cmd-add (.project self) 'cmd-clips-d&d-copy
                     :clips (.clips-dragging self)
                     :lane-ids (loop for clip in (.clips-dragging self)
                                     collect (.neko-id (gethash clip (.clip-lane-map self)))))
            ;; 移動
            (progn
              (cmd-add (.project self) 'cmd-clips-d&d-move
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

(defmethod handle-range-selecting ((self arrangement))
  ;; TODO
  (when (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
    (setf (.range-selecting-p self) nil)))

(defmethod render ((self arrangement))
  (setf (.clip-at-mouse self) nil)
  (clrhash (.clip-lane-map self))
  (compute-offset-y self)

  (ig:with-begin ("##arrangement" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (render-grid self)
    (ig:with-begin-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

      (render-time-ruler self)

      (let ((pos (@ (.time-ruler-width self) .0))
            (scroll-x (ig:get-scroll-x))
            (scroll-y (ig:get-scroll-y))
            (window-pos (ig:get-window-pos)))

        (ig:set-cursor-pos (@+ pos (@ (- scroll-x) scroll-y)))

        (ig:with-clip-rect ((@+ window-pos (@ (- (.x pos) scroll-x 3.0) .0))
                            (@+ window-pos (ig:get-window-size)))
          (render-track self (.master-track (.project self)) 0))

        (draw-vertical-line (@- (ig:get-cursor-pos) (@ .0 scroll-y)))

        (ig:set-next-item-shortcut (logior ig:+im-gui-mod-ctrl+ ig:+im-gui-key-t+))
        (when (ig:button "+" (@ *default-lane-width* (.offset-y self)))
          (cmd-add (.project self) 'cmd-track-add
                   :track-id-parent (.neko-id (.master-track (.project self)))
                   :execute-after (lambda (cmd)
                                    (let ((track (find-neko (.track-id-new cmd))))
                                      (unselect-all-tracks (.project self))
                                      (setf (.select-p track) t)))))

        (render-clip self (.master-track (.project self)) nil nil
                     (- (.time-ruler-width self) scroll-x)))

      (handle-mouse self))
    (handle-shortcut self)))

(defmethod handle-shortcut ((self arrangement))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-a+)
    (setf (.clips-selected self)
          (map-lanes (.project self) (lambda (lane acc)
                                       (append acc (.clips lane))))))
  (defshortcut (ig:+im-gui-key-delete+)
    (when (.clips-selected self)
      (cmd-add (.project self) 'cmd-clips-delete
               :clips (.clips-selected self)
               :execute-after (lambda (cmd)
                                (declare (ignore cmd))
                                (setf (.clips-selected self) nil)))))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-g+)
    (cmd-add (.project self) 'cmd-tracks-group
             :tracks (tracks-selected (.project self))))

  (shortcut-common (.project self)))

(defmethod render-clip ((self arrangement) (track track) (lane null) (clip null) x)
  (loop for lane in (.lanes track)
        do (setf x (render-clip self track lane nil x)))
  (loop for track in (.tracks track)
        do (setf x (render-clip self track nil nil x)))
  x)

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip null) x)
  (loop for clip in (.clips lane)
        do (render-clip self track lane clip x))
  (+ x (.width lane)))

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip clip) x)
  (setf (gethash clip (.clip-lane-map self)) lane)
  (let* ((draw-list (ig:get-window-draw-list))
         (y1 (time-to-local-y self (.time clip)))
         (y2 (time-to-local-y self (time-end clip)))
         (scroll-pos (@ (ig:get-scroll-x) (ig:get-scroll-y)))
         (pos1 (@ x y1))
         (pos2 (@ (+ x (.width lane)) y2))
         (window-pos (ig:get-window-pos))
         (mouse-pos (ig:get-mouse-pos)))
    (ig:set-cursor-pos pos1)
    (ig:text (format nil "  ~:[~;∞~]~a" (link-p clip) (.name clip)))

    (let ((pos1 (@+ pos1 window-pos (@- scroll-pos) (@ 2.0 .0)))
          (pos2 (@+ pos2 window-pos (@- scroll-pos) (@ -1.0 .0)))
          (color (color-selected (.color clip) (member clip (.clips-selected self)))))
      (ig:add-rect-filled draw-list
                          pos1
                          pos2
                          color
                          :rounding 3.0)
      (when (contain-p mouse-pos pos1 pos2)
        (setf (.lane-at-mouse self) lane)
        (setf (.clip-at-mouse self) clip)))))

(defmethod render-track ((self arrangement) track group-level)
  (ig:with-id (track)
    (let* ((offset-group (* (.offset-group self) (max 0 (1- group-level))))
           (pos (@+ (ig:get-cursor-pos)
                    (@ .0 offset-group)))
           (lane-width (.width (car (.lanes track))))
           (group-p (and (.tracks track) (not (typep track 'master-track))))
           (group-button-width 17.0)
           (button-width (- lane-width (if group-p group-button-width .0)))
           (button-height (- (.offset-y self) offset-group)))
      (draw-vertical-line (@- (ig:get-cursor-pos) (@ 0.0 (ig:get-scroll-y))))
      (ig:set-cursor-pos pos)
      (let ((color (color-selected (.color track) (.select-p track))))
        (ig:with-button-color (color)
          (with-renaming (track (.track-renaming self) lane-width)
            (when (ig:button (.name track) (@ button-width button-height))
              (unless (key-ctrl-p)
                (unselect-all-tracks (.project self)))
              (setf (.select-p track) t)))

          (when group-p
            (ig:same-line)
            (ig:set-cursor-pos (@+ pos (@ (- lane-width group-button-width) .0)))
            (when (ig:button (if (.tracks-show-p track) "≪" "≫")
                             (@ group-button-width button-height))
              (setf (.tracks-show-p track) (not (.tracks-show-p track)))))))

      (ig:same-line)
      (ig:set-cursor-pos (@+ pos (@ lane-width (- offset-group)))))
    (when (and (.tracks-show-p track) (.tracks track))
      (loop for x in (.tracks track)
            do (render-track self x (1+ group-level))))))

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
    (map-lanes (.project self)
               (lambda (lane width)
                 (incf width (+ (.width lane)))
                 (if (< local-x width)
                     (return-from world-x-to-lane lane)
                     (progn
                       (setf last-lane lane)
                       width)))
               .0)
    last-lane))

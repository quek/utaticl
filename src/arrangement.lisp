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

(defmethod drag-mode ((arrangement arrangement) clip)
  (let* ((mouse-pos (ig:get-mouse-pos))
         (y1 (time-to-world-y arrangement (.time clip)))
         (y2 (time-to-world-y arrangement (+ (.time clip) (.duration clip)))))
    (cond ((or (< (- y2 y1) (* +side-threshold+ 2))
               (< (+ y1 +side-threshold+)
                  (.y mouse-pos)
                  (- y2 +side-threshold+)))
           :move)
          ((<= (.y mouse-pos) (+ y1 +side-threshold+))
           :start)
          (t :end))))

(defmethod handle-click ((self arrangement))
  (aif (.clip-at-mouse self)
       (progn
         (setf (.drag-mode self) (drag-mode self it))
         (setf (.clip-target self) it)
         (when (and (not (member it (.clips-selected self)))
                    (not (key-ctrl-p)))
           (setf (.clips-selected self) (list it)))
         (unless (key-ctrl-p)
           (unselect-all-tracks (.project self)))
         (setf (.select-p (.track (.lane it))) t)
         (edit it))
       (progn
         (setf (.clips-selected self) nil)
         (let* ((time (world-y-to-time self (.y (ig:get-mouse-pos))))
                (time (time-grid-applied self time #'round)))
           (setf (.play-start (.project self)) time)))))

(defmethod handle-double-click ((self arrangement))
  (unless (.clip-at-mouse self)
    (multiple-value-bind (time lane) (world-pos-to-time-lane self (ig:get-mouse-pos))
      (setf time (time-grid-applied self time #'floor))
      (when (and (not (minusp time)) lane)
        (cmd-add (.project self) 'cmd-clip-add
                 :time time :lane-id (.neko-id lane)
                 :execute-after (lambda (cmd)
                                  (edit (find-neko (.clip-id cmd)))))))))

(defmethod handle-drag-start ((self arrangement))
  (cond ((and (.clips-selected self) (.clip-at-mouse self))
         ;; ノートの移動 or 長さ変更
         (ecase (.drag-mode self)
           (:move
            (setf (.clip-drag-offset self) (- (.y (ig:get-mouse-pos))
                                              (time-to-world-y self (.time (.clip-at-mouse self)))))
            (setf (.clips-dragging self) (mapcar #'copy (.clips-selected self)))
            (loop for clip in (.clips-dragging self)
                  for src-clip in (.clips-selected self)
                  for lane = (gethash src-clip (.clip-lane-map self))
                  do (clip-add lane clip)))
           ((:start :end)
            (setf (.clips-dragging self) (copy-list (.clips-selected self)))
            (setf (.clips-dragging-time self) (mapcar #'.time (.clips-selected self)))
            (setf (.clips-dragging-duration self) (mapcar #'.duration (.clips-selected self))))))
        (t                              ;範囲選択
         (setf (.range-selecting-mode self)
               (if (key-shift-p) :region :clip))
         (setf (.range-selecting-pos1 self) (ig:get-mouse-pos)))))

(defmethod handle-dragging ((self arrangement))
  (labels ((%time ()
             (max (time-grid-applied self
                                     (world-y-to-time self (.y (ig:get-mouse-pos)))
                                     #'round)
                  .0d0)))
    (if (not (ig:is-mouse-down ig:+im-gui-mouse-button-left+))
        ;; ドラッグの終了
        (progn
          (case (.drag-mode self)
            (:move
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
                         do (clip-delete lane clip)))))
            (:start
             (let ((delta (- (.duration (car (.clips-dragging self)))
                             (car (.clips-dragging-duration self)))))
               (loop for clip in (.clips-dragging self)
                     do (incf (.time clip) delta)
                        (decf (.duration clip) delta))
               (cmd-add (.project self) 'cmd-clips-start-change
                        :clips (.clips-dragging self)
                        :delta delta
                        :stretch-p (key-alt-p))))
            (:end
             (let ((delta (- (.duration (car (.clips-dragging self)))
                             (car (.clips-dragging-duration self)))))
               (loop for clip in (.clips-dragging self)
                     do (decf (.duration clip) delta))
               (cmd-add (.project self) 'cmd-clips-end-change
                        :clips (.clips-dragging self)
                        :delta delta
                        :stretch-p (key-alt-p)))))
          (setf (.clips-dragging self) nil))
        ;; ドラッグ中の表示
        (ecase (.drag-mode self)
          (:move
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
                     do (move dragging time lane)))))
          (:start
           (let* ((delta (- (%time) (.time (.clip-target self)))))
             (when (every (lambda (clip)
                            (plusp (- (.duration clip) delta)))
                          (.clips-dragging self))
               (loop for clip in (.clips-dragging self)
                     do (incf (.time clip) delta)
                        (decf (.duration clip) delta)))))
          (:end
           (let* ((clip (.clip-target self))
                  (delta (- (%time) (+ (.time clip) (.duration clip)))))
             (when (every (lambda (clip)
                            (plusp (+ (.duration clip) delta)))
                          (.clips-dragging self))
               (loop for clip in (.clips-dragging self)
                     do (incf (.duration clip) delta)))))))))

(defmethod handle-dragging-extern ((arrangement arrangement))
  (let ((pos (@- (ig:get-mouse-pos) (ig:get-window-pos))))
    (ig:set-cursor-pos pos)
    (ig:invisible-button +dd-extern+ (@ 0.1 0.1))
    (ig:with-drag-drop-target
      ;; マウスボタン離してなくても accept しちゃう
      (when (ig:accept-drag-drop-payload +dd-extern+  ig:+im-gui-drag-drop-flags-source-extern+)
        (setf (.dragging-source-extern arrangement) (.drop-files *app*))))))

(defmethod handle-dragging-extern-drop ((arrangement arrangement))
  (when (ig:is-mouse-down-nil ig:+im-gui-mouse-button-left+)
    (let ((path (car (.dragging-source-extern arrangement)))) ;TODO 複数ファイル
      (multiple-value-bind (time lane) (world-pos-to-time-lane arrangement (ig:get-mouse-pos))
        (setf time (time-grid-applied arrangement time #'floor))
        (when (and (not (minusp time)) lane)
          (cmd-add (.project arrangement) 'cmd-clip-audio-add
                   :time time :lane lane :path path
                   :execute-after (lambda (cmd)
                                    (edit (.clip cmd)))))))
    (setf (.dragging-source-extern arrangement) nil)))

(defmethod handle-mouse ((self arrangement))
  (if (can-handle-mouse-p self)
      (let* ((io (ig:get-io)))
        (cond ((dragging-extern-p)
               (handle-dragging-extern self))
              ((.dragging-source-extern self)
               (handle-dragging-extern-drop self))
              ((.clips-dragging self)
               (handle-dragging self))
              ((.range-selecting-mode self)
               (handle-range-selecting self))
              ((ig:is-mouse-dragging ig:+im-gui-mouse-button-left+ 0.1)
               (handle-drag-start self))
              ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
               (handle-double-click self))
              ((ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
               (handle-click self))
              ((ig:is-mouse-released ig:+im-gui-mouse-button-left+)
               (handle-mouse-released self)))
        (zoom-y-update self io))
      (progn
        (cond ((.clips-dragging self)
               (loop for dragging in (.clips-dragging self)
                     for selected in (.clips-selected self)
                     for time = (.time selected)
                     for lane = (.lane selected)
                     do (move dragging time lane))))))

  (if (.clips-dragging self)
      (ecase (.drag-mode self)
        (:move
         (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))
        ((:start :end)
         (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-resize-ns+)))
      (aif (.clip-at-mouse self)
           (ecase (drag-mode self it)
             (:move
              (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))
             ((:start :end)
              (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-resize-ns+)))
           (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))))

(defmethod handle-mouse-released ((self arrangement))
  (if (.clip-at-mouse self)
      (if (member (.clip-at-mouse self) (.clips-selected self))
          (if (key-ctrl-p)
              (setf (.clips-selected self)
                    (delete (.clip-at-mouse self) (.clips-selected self)))
              (setf (.clips-selected self) (list (.clip-at-mouse self))))
          (if (key-ctrl-p)
              (push (.clip-at-mouse self) (.clips-selected self))))))

(defmethod handle-range-selecting ((arrangement arrangement))
  (case (.range-selecting-mode arrangement)
    (:clip
     (let* ((draw-list (ig:get-window-draw-list))
            (pos1 (.range-selecting-pos1 arrangement))
            (pos2 (ig:get-mouse-pos))
            (time1 (world-y-to-time arrangement (min (.y pos1) (.y pos2))))
            (time2 (world-y-to-time arrangement (max (.y pos1) (.y pos2))))
            (lane1 (world-x-to-lane arrangement (min (.x pos1) (.x pos2))))
            (lane2 (world-x-to-lane arrangement (max (.x pos1) (.x pos2))))
            (in-selected-lane-p nil))
       (setf (.clips-selected arrangement) nil)
       (map-lanes (.project arrangement)
                  (lambda (lane acc)
                    (declare (ignore acc))
                    (when (eq lane lane1)
                      (setf in-selected-lane-p t))
                    (when in-selected-lane-p
                      (loop for clip in (.clips lane)
                            if (and (< (.time clip) time2)
                                    (< time1 (time-end clip)))
                              do (push clip (.clips-selected arrangement))))
                    (when (eq lane lane2)
                      (setf in-selected-lane-p nil)
                      (values nil t))))
       (ig:add-rect draw-list pos1 pos2
                    (.color-selecting-rect-border *theme*))))
    (:region
     ;; TODO
     ))
  (when (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
    (setf (.range-selecting-mode arrangement) nil)))

(defmethod render ((self arrangement))
  (setf (.clip-at-mouse self) nil)
  (clrhash (.clip-lane-map self))
  (compute-offset-y self)

  (ig:with-begin ("##arrangement" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (render-grid self)
    (ig:with-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

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
                                       (append acc (copy-list (.clips lane)))))))
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
         (window-size (ig:get-window-size))
         (pos1-world (@+ pos1 window-pos (@- scroll-pos)))
         (pos2-world (@+ pos2 window-pos (@- scroll-pos)))
         (pos1-visible (@ (max (.x pos1-world) (+ (.x window-pos) (.offset-x self)))
                          (max (.y pos1-world) (+ (.y window-pos) (.offset-y self)))))
         (pos2-visible (@ (min (.x pos2-world) (+ (.x window-pos) (.x window-size)))
                          (min (.y pos2-world) (+ (.y window-pos) (.y window-size)))))
         (mouse-pos (ig:get-mouse-pos)))
    (when (and (< (.x pos1-visible) (.x pos2-visible))
               (< (.y pos1-visible) (.y pos2-visible)))
      (ig:with-clip-rect (pos1-visible pos2-visible)
        (ig:set-cursor-pos pos1)
        (with-renaming (clip (.clip-renaming self) (.width lane))
          (ig:text (format nil "  ~:[~;∞~]~a" (link-p clip) (.name clip)))
          (when (contain-p mouse-pos pos1-world pos2-world)
            (ig:with-tooltip
              (ig:text (.name clip)))))

        (let ((pos1 (@+ pos1-world (@ 2.0 .0)))
              (pos2 (@+ pos2-world (@ -1.0 .0)))
              (color (color-selected (.color clip) (member clip (.clips-selected self)))))
          (ig:add-rect-filled draw-list
                              pos1
                              pos2
                              color
                              :rounding 3.0)
          (render-in-arrangement clip pos1 pos2 pos1-visible pos2-visible)
          (when (contain-p mouse-pos pos1 pos2)
            (setf (.lane-at-mouse self) lane)
            (setf (.clip-at-mouse self) clip)))))))

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
          (ig:with-drag-drop-source ()
            (ig:set-drag-drop-payload +dd-tracks+)
            (ig:text (.name track)))
          (ig:with-drag-drop-target
            (when (ig:accept-drag-drop-payload +dd-tracks+)
              (let ((tracks (tracks-selected (.project self))))
                (if (key-ctrl-p)
                    (cmd-add (.project self) 'cmd-tracks-dd-copy
                             :tracks tracks :before track)
                    (cmd-add (.project self) 'cmd-tracks-dd-move
                             :tracks tracks :before track)))))

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
  (/ (+ (- y (.y (ig:get-window-pos)) (.offset-y self))
        (ig:get-scroll-y))
     (.zoom-y self)))

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

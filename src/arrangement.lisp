(in-package :utaticl.core)

(defmethod compute-offset-y ((self arrangement))
  (labels ((f (track group-level)
             (if (and (.tracks-show-p track) (.tracks track))
                 (apply #'max (mapcar (lambda (x) (f x (1+ group-level)))
                                      (.tracks track)))
                 group-level)))
    (setf (.offset-y self)
          (+ 20.0
             (%arrangement-height-lane-param)
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
         (edit it (copy-list (.clips-selected self))))
       (progn
         (setf (.clips-selected self) nil)
         (let* ((time (max (world-y-to-time self (.y (ig:get-mouse-pos))) .0))
                (time (time-grid-applied self time #'round)))
           (setf (.play-start (.project self)) time)))))

(defmethod handle-double-click ((self arrangement))
  (unless (.clip-at-mouse self)
    (multiple-value-bind (time lane) (world-pos-to-time-lane self (ig:get-mouse-pos))
      (setf time (time-grid-applied self time #'floor))
      (when (and (not (minusp time)) lane)
        (cmd-add (.project self) 'cmd-clip-add
                 :clip (make-instance (if (.automation-param lane)
                                          'clip-automation
                                          'clip-note) :time time :color (.color lane))
                 :lane lane
                 :execute-after (lambda (cmd)
                                  (edit (.clip cmd) (list (.clip cmd)))))))))

(defmethod handle-drag-start ((self arrangement))
  (cond ((and (typep (dd-at) 'clip) (.sceen (dd-at)))
         ;; sceen-matrix からのドラッグ
         (handle-dragging-intern self))
        ((and (.clips-selected self) (.clip-at-mouse self))
         ;; ノートの移動 or 長さ変更
         (ecase (.drag-mode self)
           (:move
            (setf (.clips-dragging self) (mapcar #'copy (.clips-selected self)))
            (dd-start (.clips-selected self) (.clip-at-mouse self))
            (loop for clip in (.clips-dragging self)
                  for lane = (.lane clip)
                  do (clip-add lane clip))
            (setf (.drag-start-times self)
                  (mapcar #'.time (.clips-dragging self)))
            (setf (.drag-start-lanes self)
                  (mapcar #'.lane (.clips-dragging self)))
            (multiple-value-bind (time lane)
                (world-pos-to-time-lane self (ig:get-mouse-pos))
              (setf (.drag-offset-time self)
                    (- (.time (.clip-at-mouse self)) time))
              (setf (.drag-offset-lane self)
                    (diff (.lane (.clip-at-mouse self)) lane))))
           ((:start :end)
            (setf (.clips-dragging self)
                  (copy-list (.clips-selected self)))
            (setf (.clips-dragging-time self)
                  (mapcar #'.time (.clips-selected self)))
            (setf (.clips-dragging-duration self)
                  (mapcar #'.duration (.clips-selected self))))))
        (t                              ;範囲選択
         (setf (.range-selecting-mode self)
               (if (key-shift-p) :region :clip))
         (setf (.range-selecting-pos1 self) (ig:get-mouse-pos)))))

(defmethod handle-drag-end ((self arrangement)
                            (drag-mode (eql :move))
                            (key-ctrl-p (eql t))
                            sceen)
  (cmd-add (.project self) 'cmd-clips-d&d-copy
           :clips (.clips-dragging self)))

(defmethod handle-drag-end ((self arrangement)
                            (drag-mode (eql :move))
                            (key-ctrl-p (eql nil))
                            (sceen null))
  (cmd-add (.project self) 'cmd-clips-d&d-move
           :clips (.clips-selected self)
           :times-to (mapcar #'.time (.clips-dragging self))
           :lanes-to (mapcar #'.lane (.clips-dragging self))
           :sceens-to (mapcar #'.sceen (.clips-dragging self)))
  (loop for clip in (.clips-dragging self)
        for lane = (.lane clip)
        do (clip-delete lane clip)))

(defmethod handle-drag-end ((self arrangement)
                            (drag-mode (eql :move))
                            (key-ctrl-p (eql nil))
                            (sceen sceen))
  (cmd-add (.project self)
           'cmd-clips-d&d-move-from-sceen-matrix-to-self
           :clips-from (dd-src)
           :clips-to (.clips-dragging self)))

(defmethod handle-drag-end ((self arrangement)
                            (drag-mode (eql :start))
                            key-ctrl-p
                            sceen)
  (let ((delta (- (.duration (car (.clips-dragging self)))
                  (car (.clips-dragging-duration self)))))
    (loop for clip in (.clips-dragging self)
          do (incf (.time clip) delta)
             (decf (.duration clip) delta))
    (cmd-add (.project self) 'cmd-clips-start-change
             :clips (.clips-dragging self)
             :delta delta
             :stretch-p (key-alt-p))))

(defmethod handle-drag-end ((self arrangement)
                            (drag-mode (eql :end))
                            key-ctrl-p
                            sceen)
  (let ((delta (- (.duration (car (.clips-dragging self)))
                  (car (.clips-dragging-duration self)))))
    (loop for clip in (.clips-dragging self)
          do (decf (.duration clip) delta))
    (cmd-add (.project self) 'cmd-clips-end-change
             :clips (.clips-dragging self)
             :delta delta
             :stretch-p (key-alt-p))))

(defmethod handle-dragging ((self arrangement))
  (labels ((%time ()
             (max (time-grid-applied self
                                     (world-y-to-time self (.y (ig:get-mouse-pos)))
                                     #'round)
                  .0d0)))
    (if (not (ig:is-mouse-down ig:+im-gui-mouse-button-left+))
        ;; ドラッグの終了
        (progn
          (handle-drag-end self (.drag-mode self) (key-ctrl-p)
                           (.sceen (car (dd-src))))
          (setf (.clips-dragging self) nil))
        ;; ドラッグ中の表示
        (ecase (.drag-mode self)
          (:move
           (multiple-value-bind (time lane)
               (world-pos-to-time-lane self
                                       (@+ *mouse-pos*
                                           (@ .0 (* (.drag-offset-time self)
                                                    (.zoom-y self)))))
             (setf time (max (time-grid-applied self time #'floor) .0d0))
             (let ((delta-time (- time (car (.drag-start-times self))))
                   (delta-lane (diff lane (car (.drag-start-lanes self)))))
               (loop for dragging in (.clips-dragging self)
                     for drag-start-time in (.drag-start-times self)
                     for drag-start-lane in (.drag-start-lanes self)
                     for time = (+ drag-start-time delta-time)
                     for lane = (relative-at drag-start-lane delta-lane)
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

(defmethod handle-dragging-extern ((self arrangement))
  (let ((pos (@- (ig:get-mouse-pos) (ig:get-window-pos))))
    (ig:set-cursor-pos pos)
    (ig:invisible-button +dd-extern+ (@ 0.1 0.1))
    (ig:with-drag-drop-target
      ;; マウスボタン離してなくても accept しちゃう
      (when (ig:accept-drag-drop-payload +dd-extern+  ig:+im-gui-drag-drop-flags-source-extern+)
        (setf (.dragging-source-extern self) (dd-src))))))

(defmethod handle-dragging-extern-drop ((self arrangement))
  (when (ig:is-mouse-down-nil ig:+im-gui-mouse-button-left+)
    (let ((path (car (.dragging-source-extern self)))) ;TODO 複数ファイル
      (multiple-value-bind (time lane) (world-pos-to-time-lane self (ig:get-mouse-pos))
        (setf time (time-grid-applied self time #'floor))
        (when (and (not (minusp time)) lane)
          (cmd-add (.project self) 'cmd-clip-audio-add
                   :time time :lane lane :path path
                   :execute-after (lambda (cmd)
                                    (edit (.clip cmd) (list (.clip cmd))))))))
    (dd-reset)
    (setf (.dragging-source-extern self) nil)))

(defmethod handle-dragging-intern ((self arrangement))
  (setf (.drag-mode self) :move)
  (setf (.clips-dragging self)
        (mapcar #'copy (dd-src)))
  (loop for clip in (.clips-dragging self)
        for lane = (.lane clip)
        do (setf (.sceen clip) nil)
           (clip-add lane clip))
  (setf (.drag-start-times self)
        (mapcar #'.time (.clips-dragging self)))
  (setf (.drag-start-lanes self)
        (mapcar #'.lane (.clips-dragging self)))
  (multiple-value-bind (time lane)
      (world-pos-to-time-lane self (ig:get-mouse-pos))
    (setf (.drag-offset-time self) .0)
    (loop for clip in (.clips-dragging self)
          do (setf (.time clip) time))
    (setf (.drag-offset-lane self)
          (diff (.lane (dd-at)) lane))))

(defmethod handle-mouse ((self arrangement))
  (unless (dd-src)
    ;; sceen-matrix にドロップしたときのクリア処理
    (loop for clip in (.clips-dragging self)
          do (clip-delete (.lane clip) clip))
    (setf (.clips-dragging self) nil))
  (if (can-handle-mouse-p self)
      (let ((io (ig:get-io))
            (mouse-in-cavas-p (mouse-in-cavas-p self)))
        (cond ((dragging-extern-p)
               (handle-dragging-extern self))
              ((.dragging-source-extern self)
               (handle-dragging-extern-drop self))
              ((.clips-dragging self)
               (handle-dragging self))
              ((and mouse-in-cavas-p (.range-selecting-mode self))
               (handle-range-selecting self))
              ((and mouse-in-cavas-p (ig:is-mouse-dragging ig:+im-gui-mouse-button-left+ 0.1))
               (handle-drag-start self))
              ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
               (handle-double-click self))
              ((ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
               (handle-click self))
              ((ig:is-mouse-released ig:+im-gui-mouse-button-left+)
               (handle-mouse-released self)))
        (zoom-y-update self io))
      (when (.clips-dragging self)
        (loop for dragging in (.clips-dragging self)
              for selected in (.clips-selected self)
              for time = (.time selected)
              for lane = (.lane selected)
              do (move dragging time lane))))

  (mouse-cursor self))

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
  (case (.range-selecting-mode self)
    (:clip
     (let* ((draw-list (ig:get-window-draw-list))
            (pos1 (.range-selecting-pos1 self))
            (pos2 (ig:get-mouse-pos))
            (time1 (world-y-to-time self (min (.y pos1) (.y pos2))))
            (time2 (world-y-to-time self (max (.y pos1) (.y pos2))))
            (lane1 (world-x-to-lane self (min (.x pos1) (.x pos2))))
            (lane2 (world-x-to-lane self (max (.x pos1) (.x pos2))))
            (in-selected-lane-p nil))
       (setf (.clips-selected self) nil)
       (map-lanes (.project self)
                  (lambda (lane acc)
                    (declare (ignore acc))
                    (when (eq lane lane1)
                      (setf in-selected-lane-p t))
                    (when in-selected-lane-p
                      (loop for clip in (.clips lane)
                            if (and (< (.time clip) time2)
                                    (< time1 (time-end clip)))
                              do (push clip (.clips-selected self))))
                    (when (eq lane lane2)
                      (setf in-selected-lane-p nil)
                      (values nil t))))
       (ig:add-rect draw-list pos1 pos2
                    (.color-selecting-rect-border *theme*))))
    (:region
     (setf (.range-selecting-pos2 self) *mouse-pos*)))
  (when (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
    (setf (.range-selecting-mode self) nil)))

(defmethod mouse-cursor ((self arrangement))
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

(defmethod mouse-in-cavas-p ((self arrangement))
  (let* ((window-pos (ig:get-window-pos))
         (x1 (+ (.x window-pos) (.offset-x self)))
         (y1 (+ (.y window-pos) (.offset-y self)))
         (window-size (ig:get-window-size))
         (x2 (+ x1 (.x window-size)))
         (y2 (+ y1 (.y window-size))))
    (and (<= x1 (.x *mouse-pos*) x2)
         (<= y1 (.y *mouse-pos*) y2))))

(defmethod render ((self arrangement))
  (setf (.clip-at-mouse self) nil)
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
          (ig:with-styles ((ig:+im-gui-style-var-item-spacing+ (@ .0 .0)))
            (render-track self (.master-track (.project self)) 0)))

        (draw-vertical-line (@- (ig:get-cursor-pos) (@ .0 scroll-y)))

        (ig:set-next-item-shortcut (logior ig:+im-gui-mod-ctrl+ ig:+im-gui-key-t+))
        (when (ig:button "+" (@ *default-lane-width* (.offset-y self)))
          (cmd-add (.project self) 'cmd-track-add
                   :track-id-parent (.neko-id (.master-track (.project self)))
                   :execute-after (lambda (cmd)
                                    (let ((track (find-neko (.track-id-new cmd))))
                                      (unselect-all-tracks (.project self))
                                      (setf (.select-p track) t)))))

        (ig:with-clip-rect ((@+ window-pos (@ (- (.x pos) scroll-x 3.0) .0))
                            (@+ window-pos (ig:get-window-size)))
          (ig:with-styles ((ig:+im-gui-style-var-item-spacing+ (@ .0 .0)))
            (map-lanes *project* (lambda (lane x)
                                   (let ((track-parent (.parent (.track lane))))
                                     (if (or (null track-parent)
                                             (.tracks-show-p track-parent))
                                         (%arrangement-render-lane lane x)
                                         x)))
                       .0)))

        (render-clip self (.master-track (.project self)) nil nil
                     (- (.time-ruler-width self) scroll-x)))

      (handle-mouse self))
    (handle-shortcut self)))

(defmethod handle-shortcut ((self arrangement))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-a+)
    (setf (.clips-selected self)
          (map-lanes (.project self) (lambda (lane acc)
                                       (append acc (copy-list (.clips lane)))))))
  (awhen (.clips-selected self)
    (defshortcut (ig:+im-gui-key-e+)
      (edit (car it) (reverse it))))
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
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-l+)
    (cmd-add *project* 'cmd-lane-add :track (.target-track *project*)))

  (shortcut-common (.project self)))

(defmethod render-clip ((self arrangement) (track track) (lane null) (clip null) x)
  (loop for lane in (.lanes track)
        for line = (lambda ())
          then (lambda ()
                 (draw-vertical-line (@ x (.offset-y self))))
        for x2 = (+ x (.width lane))
        with window-pos-y = (.y (ig:get-window-pos))
        with y1 = (+ window-pos-y
                     (.offset-y self))
        with y2 = (+ window-pos-y (ig:get-window-height))
        do (funcall line)
           (dd-drop lane (@@ x y1 x2 y2))
           (setf x (render-clip self track lane nil x)))
  (when (.tracks-show-p track)
    (loop for track in (.tracks track)
          do (setf x (render-clip self track nil nil x))))
  x)

(defmethod dd-drop-at ((lane lane) (param param))
  (setf (.automation-param lane) param)
  (dd-reset))

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip null) x)
  (loop for clip in (.clips lane)
        do (render-clip self track lane clip x))
  (+ x (.width lane)))

(defmethod render-clip ((self arrangement) (track track) (lane lane) (clip clip) x)
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
          (ig:set-cursor-pos pos1)
          (ig:invisible-button "##" (@- pos2 pos1))
          (when (and (contain-p mouse-pos pos1-world pos2-world)
                     (null (dd-src)))
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
           (track-width (.width track))
           (group-p (and (.tracks track) (not (typep track 'master-track))))
           (group-button-width 17.0)
           (button-width (- track-width (if group-p group-button-width .0)))
           (button-height (- (.offset-y self) offset-group
                             (%arrangement-height-lane-param))))
      (draw-vertical-line (@- (ig:get-cursor-pos) (@ 0.0 (ig:get-scroll-y))))
      (ig:set-cursor-pos pos)
      (let ((color (color-selected (.color track) (.select-p track))))
        (ig:with-button-color (color)
          (with-renaming (track (.track-renaming self) track-width)
            (when (ig:button (.name track) (@ button-width button-height))
              (unless (key-ctrl-p)
                (unselect-all-tracks (.project self)))
              (setf (.select-p track) t))
            (ig:with-popup-context-item ()
              (when (ig:menu-item "Copy" :shortcut "C-c")
                )
              (when (ig:menu-item "Paste" :shortcut "C-v")
                )
              (when (ig:menu-item "Add Lane" :shortcut "C-l")
                (cmd-add *project* 'cmd-lane-add
                         :track track))))
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
            (ig:set-cursor-pos (@+ pos (@ (- track-width group-button-width) .0)))
            (when (ig:button (if (.tracks-show-p track) "≪" "≫")
                             (@ group-button-width button-height))
              (setf (.tracks-show-p track) (not (.tracks-show-p track)))))))

      (ig:same-line)
      (ig:set-cursor-pos (@+ pos (@ track-width (- offset-group)))))
    (when (and (.tracks-show-p track) (.tracks track))
      (loop for x in (.tracks track)
            do (render-track self x (1+ group-level))))))

(defmethod .track-height ((self arrangement) track)
  ;; TODO
  60.0)

(defmethod world-pos-to-time-lane ((self arrangement) pos)
  (let ((time (world-y-to-time self (.y pos)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %arrangement-render-lane (lane x)
  (let ((param (.automation-param lane)))
    (when param
      (let ((pos (@ (+ x (.offset-x (.arrangement *project*)))
                    (- (.offset-y (.arrangement *project*))
                       (%arrangement-height-lane-param)))))
        (ig:set-cursor-pos pos)
        (ig:with-id (lane)
          (ig:with-group
            (ig:set-next-item-width (.width lane))
            (ig:set-cursor-pos-x (+ (.x pos) 4.0))
            (ig:text (.name param))
            (ig:set-next-item-width (.width lane))
            (ig:drag-double
             "##default-value" (.automation-default-value lane)
             :min 0d0 :max 1d0 :speed .01
             :format (value-text param (.automation-default-value lane))))))))
  (+ x (.width lane)))

(defun %arrangement-height-lane-param ()
  40.0)

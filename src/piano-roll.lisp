(in-package :utaticl.core)

(defmethod drag-mode ((self piano-roll) note)
  (let* ((mouse-pos (ig:get-mouse-pos))
         (y1 (time-to-world-y self (.time note)))
         (y2 (time-to-world-y self (+ (.time note) (.duration note)))))
    (cond ((or (< (- y2 y1) (* +side-threshold+ 2))
               (< (+ y1 +side-threshold+)
                  (.y mouse-pos)
                  (- y2 +side-threshold+)))
           :move)
          ((<= (.y mouse-pos) (+ y1 +side-threshold+))
           :start)
          (t :end))))

(defmethod handle-click ((self piano-roll))
  (if (range-selecting-p self)
      nil
      (let ((note-at-mouse (.note-at-mouse self)))
        (if note-at-mouse
            (unless (key-alt-p)    ;alt は region 選択しようとしているとき
              (setf (.drag-mode self) (drag-mode self note-at-mouse))
              (setf (.note-target self) note-at-mouse)
              (setf (.note-default-duration self) (.duration note-at-mouse))
              (unless (range-selecting-p self)
                (when (and (not (member note-at-mouse (.notes-selected self)))
                           (not (key-ctrl-p)))
                  (setf (.notes-selected self) (list note-at-mouse)))))
            (progn
              (setf (.notes-selected self) nil)
              (setf (.range-selecting-pos1 self) nil)
              (setf (.range-selecting-pos2 self) nil))))))

(defmethod handle-double-click ((self piano-roll))
  (if (.note-at-mouse self)
      (cmd-add (.project self) 'cmd-note-delete
               :clip-id (.neko-id (.clip self))
               :note (.note-at-mouse self))
      (multiple-value-bind (time key) (world-pos-to-time-key self (ig:get-mouse-pos))
        (setf time (time-grid-applied self time #'floor))
        (when (and (not (minusp time)) key)
          (let* ((duration (.note-default-duration self))
                 (sys-window-pos (sys-window-pos *app*))
                 (x (round (+ (key-to-world-x self key)
                              (/ (.zoom-x self) ;key width
                                 2)
                              (.x sys-window-pos))))
                 (y (round (+ (time-to-world-y self (+ time duration))
                              (.y sys-window-pos)))))
            (setf (.note-add-pos self) (ig:get-mouse-pos))
            (sys-set-cursor-pos x y)

            (cmd-add (.project self) 'cmd-note-add
                     :clip-id (.neko-id (.clip self))
                     :time time
                     :key key
                     :duration duration
                     :execute-after (lambda (cmd)
                                      ;; そのままドラッグで長さを変えられる
                                      (let ((note (find-neko (.note-id cmd))))
                                        (setf (.note-target self) note)
                                        (setf (.notes-selected self) (list note))
                                        (setf (.drag-mode self) :end)))))))))


(defmethod handle-drag-start ((self piano-roll))
  (cond ((key-alt-p)
         (setf (.range-selecting-mode self) :region)
         (setf (.range-selecting-pos1 self) *mouse-pos*))
        ((range-selecting-p self)
         ;; リージョンのドラッグ開始
         (setf (.range-dragging self)
               (range-selecting-region-time-key
                self
                (.range-selecting-pos1 self)
                (.range-selecting-pos2 self)))
         (setf (.notes-dragging self)
               (range-copy (.clip self)
                           (range-selecting-region-time-key
                            self
                            (.range-selecting-pos1 self)
                            (.range-selecting-pos2 self))))
         (setf (.drag-mode self) :move)
         (setf (.note-drag-offset self)
               (@- (.range-selecting-pos1 self) *mouse-pos*))
         (dd-start-force self (mapcar #'copy (.notes-dragging self))
                   *mouse-pos*))
        ((.notes-selected self)
         ;; 選択ノートのドラッグ開始
         (progn
           (ecase (.drag-mode self)
             (:move
              (setf (.note-drag-offset self)
                    (@- (ig:get-mouse-pos)
                        (@ (key-to-world-x self (.key (.note-target self)))
                           (time-to-world-y self (.time (.note-target self))))))
              (setf (.notes-dragging self) (mapcar #'copy (.notes-selected self)))
              (loop for note in (.notes-dragging self)
                    for src-note in (.notes-selected self)
                    do (note-add (.clip self) note)))
             ((:start :end)
              (setf (.notes-dragging self) (copy-list (.notes-selected self)))
              (setf (.notes-dragging-time self) (mapcar #'.time (.notes-selected self)))
              (setf (.notes-dragging-duration self) (mapcar #'.duration (.notes-selected self)))))))
        (t
         (setf (.range-selecting-mode self) :note)
         (setf (.range-selecting-pos1 self) *mouse-pos*))))

(defmethod handle-dragging ((self piano-roll))
  (labels ((%time ()
             (max (time-grid-applied self
                                     (world-y-to-time self (.y (ig:get-mouse-pos)))
                                     #'round)
                  .0d0)))
    (if (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
        ;; ドラッグの終了
        (if (range-selecting-p self)
            (progn
              (loop for note in (.notes-dragging self)
                    do (note-delete (.clip self) note))
              (cmd-add (.project self) (if (key-ctrl-p)
                                           'cmd-range-dd-copy
                                           'cmd-range-dd-move)
                       :clip (.clip self)
                       :range-src (range-src-time-key self)
                       :range-dst (range-dst-time-key self))
              (range-selecting-clear self))
            (progn
              (ecase (.drag-mode self)
                (:move
                 (if (key-ctrl-p)
                     ;; 複製
                     (cmd-add (.project self) 'cmd-notes-dd-copy
                              :notes (.notes-dragging self)
                              :clip-id (.neko-id (.clip self)))
                     ;; 移動
                     (progn
                       (cmd-add (.project self) 'cmd-notes-dd-move
                                :notes (.notes-selected self)
                                :times-to (mapcar #'.time (.notes-dragging self))
                                :keys-to (mapcar #'.key (.notes-dragging self)))
                       (loop for note in (.notes-dragging self)
                             do (note-delete (.clip self) note)))))
                (:start
                 (let ((delta (- (.duration (car (.notes-dragging self)))
                                 (car (.notes-dragging-duration self)))))
                   (loop for note in (.notes-dragging self)
                         do (incf (.time note) delta)
                            (decf (.duration note) delta))
                   (cmd-add (.project self) 'cmd-notes-start-change
                            :notes (.notes-dragging self)
                            :delta delta)))
                (:end
                 (let ((delta (- (.duration (car (.notes-dragging self)))
                                 (car (.notes-dragging-duration self)))))
                   (loop for note in (.notes-dragging self)
                         do (decf (.duration note) delta))
                   (cmd-add (.project self) 'cmd-notes-end-change
                            :notes (.notes-dragging self)
                            :delta delta)
                   (setf (.note-default-duration self)
                         (+ (.duration (.note-target self))
                            delta)))
                 ;; ノート追加後のドラッグで duration 変更からカーソル位置を戻す。
                 (swhen (.note-add-pos self)
                   (let ((sys-window-pos (sys-window-pos *app*)))
                     (sys-set-cursor-pos (round (+ (.x it) (.x sys-window-pos)))
                                         (round (+ (.y it) (.y sys-window-pos)))))
                   (setf it nil))))

              (setf (.notes-dragging self) nil)))
        ;; ドラッグ中の表示
        (if (range-selecting-p self)
            (let* ((pos-delta (@- *mouse-pos* (dd-at)))
                   (pos1 (@+ (.range-selecting-pos1 self) pos-delta))
                   (pos2 (@+ (.range-selecting-pos2 self) pos-delta))
                   (time-delta (- (world-y-to-time self (.y pos1))
                                  (world-y-to-time self (.y (.range-selecting-pos1 self)))))
                   (time-delta (time-grid-applied self time-delta
                                                  (if (minusp time-delta)
                                                      #'floor
                                                      #'ceiling)))
                   (key-delta (- (world-x-to-key self (.x pos1))
                                 (world-x-to-key self (.x (.range-selecting-pos1 self))))))
              (multiple-value-bind (pos1-grid pos2-grid)
                  (range-selecting-region self pos1 pos2)
                (ig:add-rect-filled (ig:get-window-draw-list) pos1-grid pos2-grid
                                    (.color-selected-region *theme*)))
              (loop for note-dragging in (.notes-dragging self)
                    for note-src in (dd-src)
                    for time = (max .0 (+ (.time note-src) time-delta))
                    for key = (min 127 (max 0 (+ (.key note-src) key-delta)))
                    do (move note-dragging time key)))
            (ecase (.drag-mode self)
              (:move
               (multiple-value-bind (time key)
                   (world-pos-to-time-key self (@- *mouse-pos*
                                                   (@ .0 (.y (.note-drag-offset self)))))
                 (setf time (max (time-grid-applied self time #'floor) .0d0))
                 (let ((delta-time (- time (.time (.note-target self))))
                       (delta-key (- key (.key (.note-target self)))))
                   (loop for dragging in (.notes-dragging self)
                         for selected in (.notes-selected self)
                         for time = (+ (.time selected) delta-time)
                         for key = (+ (.key selected) delta-key)
                         do (move dragging time key)))))
              (:start
               (let* ((delta (- (%time) (.time (.note-target self)))))
                 (when (every (lambda (note)
                                (plusp (- (.duration note) delta)))
                              (.notes-dragging self))
                   (loop for note in (.notes-dragging self)
                         do (incf (.time note) delta)
                            (decf (.duration note) delta)))))
              (:end
               (let* ((note (.note-target self))
                      (delta (- (%time) (+ (.time note) (.duration note)))))
                 (when (every (lambda (note)
                                (plusp (+ (.duration note) delta)))
                              (.notes-dragging self))
                   (loop for note in (.notes-dragging self)
                         do (incf (.duration note) delta))))))))))

(defmethod handle-mouse ((self piano-roll))
  (when (can-handle-mouse-p self)
    (let* ((io (ig:get-io)))
      (cond ((.notes-dragging self)
             (handle-dragging self))
            ((.range-dragging self)
             (handle-range-dragging self))
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
      (zoom-x-update self io)
      (zoom-y-update self io)))

  (when (range-selecting-p self)
    (let ((draw-list (ig:get-window-draw-list)))
      (multiple-value-bind (pos1 pos2)
          (range-selecting-region self
                                  (.range-selecting-pos1 self)
                                  (.range-selecting-pos2 self))
        (ig:add-rect-filled draw-list pos1 pos2
                            (.color-selected-region *theme*)))))

  (when (can-handle-mouse-p self)
    (if (.notes-dragging self)
        (ecase (.drag-mode self)
          (:move
           (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))
          ((:start :end)
           (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-resize-ns+)))
        (aif (.note-at-mouse self)
             (ecase (drag-mode self it)
               (:move
                (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))
               ((:start :end)
                (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-resize-ns+)))
             (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+)))))

(defmethod handle-range-dragging ((piano-roll piano-roll))
  (if (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
      (progn
        (range-selecting-clear piano-roll))
      (progn
        (multiple-value-bind (time key)
            (world-pos-to-time-key piano-roll
                                   (@- (ig:get-mouse-pos)
                                       (.note-drag-offset piano-roll)))
          (setf time (max (time-grid-applied piano-roll time #'floor) .0d0))
          (let ((delta-time (- time (.time (.note-target piano-roll))))
                (delta-key (- key (.key (.note-target piano-roll)))))
            (loop for dragging in (.notes-dragging piano-roll)
                  for selected in (.notes-selected piano-roll)
                  for time = (+ (.time selected) delta-time)
                  for key = (+ (.key selected) delta-key)
                  do (move dragging time key)))))))

(defmethod handle-mouse-released ((self piano-roll))
  (if (range-selecting-p self)
      (range-selecting-clear self)
      (if (.note-at-mouse self)
          (if (member (.note-at-mouse self) (.notes-selected self))
              (if (key-ctrl-p)
                  (setf (.notes-selected self)
                        (delete (.note-at-mouse self) (.notes-selected self)))
                  (setf (.notes-selected self) (list (.note-at-mouse self))))
              (if (key-ctrl-p)
                  (push (.note-at-mouse self) (.notes-selected self)))))))

(defmethod handle-range-selecting ((self piano-roll))
  (case (.range-selecting-mode self)
    (:note
     (let* ((draw-list (ig:get-window-draw-list))
            (pos1 (.range-selecting-pos1 self))
            (pos2 (ig:get-mouse-pos))
            (time1 (world-y-to-time self (min (.y pos1) (.y pos2))))
            (time2 (world-y-to-time self (max (.y pos1) (.y pos2))))
            (key1 (world-x-to-key self (min (.x pos1) (.x pos2))))
            (key2 (world-x-to-key self (max (.x pos1) (.x pos2)))))
       (setf (.notes-selected self)
             (loop for note in (.notes (.seq (.clip self)))
                   if (and (< (.time note) time2)
                           (< time1 (time-end note))
                           (<= key1 (.key note) key2))
                     collect note))
       (ig:add-rect draw-list pos1 pos2
                    (.color-selecting-rect-border *theme*))))
    (:region
     (setf (.range-selecting-pos2 self) *mouse-pos*)))

  (when (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
    (setf (.range-selecting-mode self) nil)))

(defmethod handle-shortcut ((self piano-roll))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-a+)
    (setf (.notes-selected self) (copy-list (.notes (.seq (.clip self))))))
  (defshortcut (ig:+im-gui-key-delete+)
    (when (.notes-selected self)
      (cmd-add (.project self) 'cmd-notes-delete
               :notes (.notes-selected self)
               :clip (.clip self))))
  (defshortcut (ig:+im-gui-key-d+)
    (if (range-selecting-p self)
        (cmd-add (.project self) 'cmd-notes-duplicate-region
                 :pos1 (.range-selecting-pos1 self)
                 :pos2 (.range-selecting-pos2 self)
                 :notes (.notes-selected self))
        (when (.notes-selected self)
          (cmd-add (.project self) 'cmd-notes-duplicate
                   :notes (.notes-selected self)
                   :clip (.clip self)
                   :execute-after (lambda (cmd)
                                    (setf (.notes-selected self)
                                          (.notes-undo cmd)))))))
  (defshortcut (ig:+im-gui-key-x+)
    (setf (.view-fit-request-p self) t))
  (shortcut-common (.project self)))

(defmethod key-to-local-x ((self piano-roll) key)
  (+ (* (.zoom-x self) key)
     (.offset-x self)))

(defmethod key-to-world-x ((self piano-roll) key)
  (+ (key-to-local-x self key)
     (.x (ig:get-window-pos))
     (- (ig:get-scroll-x))))

(defmethod playhead-y ((self piano-roll))
  (let* ((clip (.clip self))
         (offset-start (.offset-start clip))
         (duration-seq (.duration (.seq clip))))
    (if (.sceen clip)
        (time-to-local-y self (rem (.play-start *project*)
                                   (.duration clip)))
        (- (time-to-local-y
            self
            (max (- (.play-start *project*)
                    (.time clip)
                    (- offset-start)
                    (* (floor (/ (+ (.play-start *project*)
                                    (- (.time clip))
                                    offset-start)
                                 duration-seq))
                       duration-seq))
                 0d0))
           *scroll-y*))))

(defmethod range-dst ((piano-roll piano-roll))
  (let* ((pos-delta (@- *mouse-pos* (dd-at)))
         (pos1 (@+ (.range-selecting-pos1 piano-roll) pos-delta))
         (pos2 (@+ (.range-selecting-pos2 piano-roll) pos-delta)))
    (multiple-value-bind (pos1-grid pos2-grid)
        (range-selecting-region piano-roll pos1 pos2)
      `(,@pos1-grid ,@pos2-grid))))

(defmethod range-dst-time-key ((piano-roll piano-roll))
  (let* ((pos-delta (@- *mouse-pos* (dd-at)))
         (pos1 (@+ (.range-selecting-pos1 piano-roll) pos-delta))
         (pos2 (@+ (.range-selecting-pos2 piano-roll) pos-delta)))
    (multiple-value-bind (pos1-grid pos2-grid)
        (range-selecting-region piano-roll pos1 pos2)
      (range-selecting-region-time-key
       piano-roll
       pos1-grid pos2-grid))))

(defmethod range-selecting-clear ((piano-roll piano-roll))
  (setf (.notes-selected piano-roll) nil)
  (setf (.range-selecting-pos1 piano-roll) nil)
  (setf (.range-selecting-pos2 piano-roll) nil)
  (setf (.range-dragging piano-roll) nil))

(defmethod range-selecting-p ((piano-roll piano-roll))
  (and (.range-selecting-pos1 piano-roll)
       (.range-selecting-pos2 piano-roll)))

(defmethod range-selecting-region-time-key ((self piano-roll) pos1 pos2)
  (multiple-value-bind (time1 key1)
      (world-pos-to-time-key self pos1)
    (setf time1 (time-grid-applied self time1 (if (< (.y pos1)
                                                     (.y pos2))
                                                  #'floor
                                                  #'ceiling)))
    (multiple-value-bind (time2 key2) (world-pos-to-time-key self pos2)
      (setf time2 (time-grid-applied self time2 (if (< (.y pos1)
                                                       (.y pos2))
                                                    #'ceiling
                                                    #'floor)))
      (let ((time1 (min time1 time2))
            (key1 (min key1 key2))
            (time2 (max time1 time2))
            (key2 (1+ (max key1 key2))))
        (list time1 key1 time2 key2)))))

(defmethod range-selecting-region ((self piano-roll) pos1 pos2)
  (destructuring-bind (time1 key1 time2 key2)
      (range-selecting-region-time-key self pos1 pos2)
    (let* ((x1 (key-to-world-x self key1))
           (y1 (time-to-world-y self time1))
           (x2 (key-to-world-x self key2))
           (y2 (time-to-world-y self time2))
           (pos1 (@ x1 y1))
           (pos2 (@ x2 y2)))
      (values pos1 pos2))))

(defmethod range-src ((piano-roll piano-roll))
  `(,@(.range-selecting-pos1 piano-roll)
    ,@(.Range-selecting-pos2 piano-roll)))

(defmethod range-src-time-key ((piano-roll piano-roll))
  (range-selecting-region-time-key
   piano-roll
   (.range-selecting-pos1 piano-roll)
   (.range-selecting-pos2 piano-roll)))

(defmethod render ((self piano-roll))
  (setf (.note-at-mouse self) nil)

  (ig:with-begin ("##piano-roll" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (render-grid self)
    (loop for clip in (.clips self)
          for selected = (eq clip (.clip self))
          with first-p = t
          do (if first-p
                 (setf first-p nil)
                 (ig:same-line))
             (when (button-toggle (.name clip) selected)
               (setf (.clip self) clip)
               (setf (.notes-selected self) nil)))
    (ig:with-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
      (with-window-info (self)
        (render-time-ruler self)

        (let ((window-pos (ig:get-window-pos))
              (window-size (ig:get-window-size)))
          (ig:with-clip-rect ((@+ window-pos (@ (.offset-x self) .0))
                              (@- (@+ window-pos window-size)
                                  (@ *scrollbar-size* *scrollbar-size*)))
            (render-keyboard self))
          (ig:with-clip-rect ((@+ window-pos (@ (.offset-x self) (.offset-y self)))
                              (@- (@+ window-pos window-size)
                                  (@ *scrollbar-size* *scrollbar-size*)))
            (loop for clip in (.clips self)
                  do (render-notes self clip))))

        (swhen (.view-fit-request-p self)
          (setf it (view-fit self)))

        (handle-mouse self)))
    (handle-shortcut self)))

(defmethod render-keyboard ((self piano-roll))
  (loop with draw-list = (ig:get-window-draw-list)
        with window-pos = (ig:get-window-pos)
        with window-height = (ig:get-window-height)
        with key-width = (.zoom-x self)
        for key from +c-1+ to +g9+
        for name = (midi-key-name key)
        for black-p = (or (alexandria:ends-with #\# name)
                          (alexandria:ends-with #\b name))
        for text-color = (if black-p (color #xff #xff #xff #xc0) (color #x00 #x00 #x00 #xc0))
        for bg-color = (if black-p (color #x00 #x00 #x00 #xc0) (color #xff #xff #xff #xc0))
        for x-world = (key-to-world-x self key)
        for pos1 = (@ x-world (.y window-pos))
        for pos2 = (@+ pos1 (@ key-width (.offset-y self)))
        do (ig:add-rect-filled draw-list pos1 pos2 bg-color)
           (unless black-p
             (ig:add-rect-filled draw-list (@+ pos1 (@ .0 (.offset-y self)))
                                 (@+ pos2 (@ .0 window-height))
                                 (color #xff #xff #xff #x0d)))
           (ig:add-line draw-list pos1 (@+ pos1 (@ .0 window-height)) (.color-line *theme*))
           (when (text-show-p self)
             (let ((pos-text (@ (key-to-local-x self key) (ig:get-scroll-y))))
               (when (and (<= .0 (.x pos-text)) (<= .0 (.y pos-text)))
                 (ig:set-cursor-pos pos-text)
                 (ig:with-button-color ((color 0 0 0 0))
                   (ig:push-style-color-u32 ig:+im-gui-col-text+ text-color)
                   (ig:button name)
                   (ig:pop-style-color 1)))))
        finally (progn                  ;height 確保のために
                  (ig:set-cursor-pos (@- pos2 window-pos))
                  (ig:text ""))))

(defmethod render-notes ((self piano-roll) (clip clip-note))
  (loop with draw-list = (ig:get-window-draw-list)
        with key-width = (.zoom-x self)
        with mouse-pos = (ig:get-mouse-pos)
        for note in (.notes (.seq clip))
        for x = (key-to-world-x self (.key note))
        for y = (time-to-world-y self (.time note))
        for pos1 = (@ x y)
        for pos2 = (@+ pos1 (@ key-width
                               (coerce (* (.duration note) (.zoom-y self)) 'single-float)))
        for color-weight = (if (eq (.clip self) clip)
                               1.0 0.5)
        do (ig:with-id (note)
             (ig:add-rect-filled draw-list
                                 (@+ pos1 (@ 2.0 .0))
                                 (@- pos2 (@ 1.0 .0))
                                 (color*
                                  (color-selected (.color note) (member note (.notes-selected self)))
                                  1.0 color-weight)
                                 :rounding 2.5)
             (when (text-show-p self)
               (ig:add-text draw-list (@+ pos1 (@ 2.0 4.0))
                            (color* (.color-text *theme*) 1.0 color-weight)
                            (.name note)))
             (when (contain-p mouse-pos pos1 pos2)
               (setf (.note-at-mouse self) note)))))

(defmethod text-show-p ((self piano-roll))
  (> (.zoom-y self) (.threshold-text-hide self)))

(defmethod view-fit ((self piano-roll))
  (let ((notes (.notes (.seq (.clip self)))))
    (if notes
        (multiple-value-bind (key-max key-min)
            (loop for note in notes
                  for key = (.key note)
                  maximize key into max
                  minimize key into min
                  finally (return (values max min)))
          (when (< (- key-max key-min) 12)
            (incf key-max (ceiling (/ (- 12 (- key-max key-min)) 2)))
            (decf key-min (ceiling (/ (- 12 (- key-max key-min)) 2))))
          (let* ((scrollbar-size (plus-c:c-ref (ig:get-style) ig:im-gui-style :scrollbar-size))
                 (zoom-x (/ (- (ig:get-window-width) (.offset-x self) scrollbar-size)
                               (+ (- key-max key-min) 3.0))))
            (if (/= (.zoom-x self) zoom-x)
                (progn
                  (log:debug zoom-x)
                  (setf (.zoom-x self) zoom-x)
                  t)
                (let ((scroll-x (max .0
                                     (- (key-to-local-x self (1- key-min)) (.offset-x self)))))
                  (ig:set-scroll-x-float scroll-x)
                  nil))))
        (progn
          (ig:set-scroll-x-float (key-to-local-x self +a3+))
          nil))))

(defmethod world-pos-to-time-key ((self piano-roll) pos)
  (let* ((time (world-y-to-time self (.y pos)))
         (key (world-x-to-key self (.x pos))))
    (values time key)))

(defmethod world-x-to-key ((self piano-roll) x)
  (let ((local-x (+ (- x (.x (ig:get-window-pos)) (.offset-x self))
                    (ig:get-scroll-x))))
    (floor (/ local-x (.zoom-x self)))))

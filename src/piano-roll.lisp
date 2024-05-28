(in-package :dgw)

(defmethod drag-mode ((self piano-roll) note)
  (let* ((mouse-pos (ig:get-mouse-pos))
         (x1 (time-to-world-x self (.time note)))
         (x2 (time-to-world-x self (+ (.time note) (.duration note)))))
    (cond ((or (< (- x2 x1) (* +side-threshold+ 2))
               (< (+ x1 +side-threshold+)
                  (.x mouse-pos)
                  (- x2 +side-threshold+)))
           :move)
          ((<= (.x mouse-pos) (+ x1 +side-threshold+))
           :start)
          (t :end))))

(defmethod handle-click ((self piano-roll))
  (if (.note-at-mouse self)
      (let ((note-at-mouse (.note-at-mouse self)))
        (when (and (not (member note-at-mouse (.notes-selected self)))
                   (not (key-ctrl-p)))
          (setf (.notes-selected self) (list note-at-mouse)))
        (setf (.note-target self) note-at-mouse)
        (setf (.note-default-duration self) (.duration note-at-mouse))
        (setf (.drag-mode self) (drag-mode self note-at-mouse)))
      (setf (.notes-selected self) nil)))

(defmethod handle-double-click ((self piano-roll))
  (if (.note-at-mouse self)
      (cmd-add *project* 'cmd-note-delete
               :clip-id (.neko-id (.clip self))
               :note (.note-at-mouse self))
      (multiple-value-bind (time key) (world-pos-to-time-key self (ig:get-mouse-pos))
        (setf time (time-grid-applied self time :floor))
        (when (and (not (minusp time)) key)
          (cmd-add *project* 'cmd-note-add
                   :clip-id (.neko-id (.clip self))
                   :time time
                   :key key
                   :duration (.note-default-duration self)
                   :execute-after (lambda (cmd)
                                    ;; そのままドラッグで長さを変えられる
                                    (let ((note (find-neko (.note-id cmd))))
                                      (setf (.note-target self) note)
                                      (setf (.notes-selected self) (list note))
                                      (setf (.drag-mode self) :end))))))))

(defmethod handle-drag-start ((self piano-roll))
  (if (.notes-selected self)
      (progn
        (ecase (.drag-mode self)
          (:move
           (setf (.note-drag-offset self) (- (.x (ig:get-mouse-pos))
                                             (time-to-world-x self (.time (.note-target self)))))
           (setf (.notes-dragging self) (mapcar #'copy (.notes-selected self)))
           (loop for note in (.notes-dragging self)
                 for src-note in (.notes-selected self)
                 do (note-add (.clip self) note)))
          ((:start :end)
           (setf (.notes-dragging self) (.notes-selected self))
           (setf (.notes-dragging-time self) (mapcar #'.time (.notes-selected self)))
           (setf (.notes-dragging-duration self) (mapcar #'.duration (.notes-selected self))))))
      ;; 範囲選択
      (setf (.range-selecting-p self) t)))

(defmethod handle-dragging ((self piano-roll))
  (labels ((%time ()
             (max (time-grid-applied
                   self
                   (world-x-to-time self (.x (ig:get-mouse-pos)))
                   :round)
                  .0d0)))
    (if (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
        ;; ドラッグの終了
        (progn
          (ecase (.drag-mode self)
            (:move
             (if (key-ctrl-p)
                 ;; 複製
                 (cmd-add *project* 'cmd-notes-d&d-copy
                          :notes (.notes-dragging self)
                          :clip-id (.neko-id (.clip self)))
                 ;; 移動
                 (progn
                   (cmd-add *project* 'cmd-notes-d&d-move
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
               (cmd-add *project* 'cmd-notes-start-change
                        :notes (.notes-dragging self)
                        :delta delta)))
            (:end
             (let ((delta (- (.duration (car (.notes-dragging self)))
                             (car (.notes-dragging-duration self)))))
               (loop for note in (.notes-dragging self)
                     do (decf (.duration note) delta))
               (cmd-add *project* 'cmd-notes-end-change
                        :notes (.notes-dragging self)
                        :delta delta)
               (setf (.note-default-duration self)
                     (+ (.duration (.note-target self))
                        delta)))))
          (setf (.notes-dragging self) nil))
        ;; ドラッグ中の表示
        (ecase (.drag-mode self)
          (:move
           (multiple-value-bind (time key)
               (world-pos-to-time-key self (@- (ig:get-mouse-pos)
                                               (@ (.note-drag-offset self) .0)))
             (setf time (max (time-grid-applied self time :floor) .0d0))
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
                     do (incf (.duration note) delta)))))))))

(defmethod handle-mouse ((self piano-roll))
  (let* ((io (ig:get-io)))
    (cond ((.notes-dragging self)
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
    (zoom-x-update self io)
    (zoom-y-update self io))

  (if (.notes-dragging self)
      (ecase (.drag-mode self)
        (:move
         (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))
        ((:start :end)
         (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-resize-ew+)))
      (aif (.note-at-mouse self)
           (ecase (drag-mode self it)
             (:move
              (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))
             ((:start :end)
              (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-resize-ew+)))
           (ig:set-mouse-cursor ig:+im-gui-mouse-cursor-arrow+))))

(defmethod handle-mouse-released ((self piano-roll))
  (if (.note-at-mouse self)
      (if (member (.note-at-mouse self) (.notes-selected self))
          (if (key-ctrl-p)
              (setf (.notes-selected self)
                    (delete (.note-at-mouse self) (.notes-selected self)))
              (setf (.notes-selected self) (list (.note-at-mouse self))))
          (if (key-ctrl-p)
              (push (.note-at-mouse self) (.notes-selected self))))))

(defmethod handle-range-selecting ((self piano-roll))
  ;; TODO
  (when (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
    (setf (.range-selecting-p self) nil)))

(defmethod key-to-local-y ((self piano-roll) key)
  (+ (* (.zoom-y self) (- 127 key))
     (.offset-y self)))

(defmethod key-to-world-y ((self piano-roll) key)
  (+ (key-to-local-y self key)
     (.y (ig:get-window-pos))
     (- (ig:get-scroll-y))))

(defmethod render ((self piano-roll))
  (setf (.note-at-mouse self) nil)

  (ig:with-begin ("##piano-roll" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (render-grid self)
    (ig:text (.name (.clip self)))
    (ig:with-begin-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

      (render-time-ruler self)

      (let ((window-pos (ig:get-window-pos))
            (window-size (ig:get-window-size)))
        (ig:with-clip-rect ((@+ window-pos (@ .0 (.offset-y self)))
                            (@+ window-pos window-size))
          (render-keyboard self))
        (ig:with-clip-rect ((@+ window-pos (@ (.offset-x self) (.offset-y self)))
                            (@+ window-pos window-size))
          (render-notes self)))

      (swhen (.render-first-p self)
        (setf it (view-fit self)))

      (handle-mouse self))
    (shortcut-common)))

(defmethod render-keyboard ((self piano-roll))
  (setf (.offset-x self) 30.0)
  (loop with draw-list = (ig:get-window-draw-list)
        with window-pos = (ig:get-window-pos)
        with window-width = (ig:get-window-width)
        with key-height = (.zoom-y self)
        for key from +g9+ downto +c-1+
        for name = (midi-key-name key)
        for black-p = (or (alexandria:ends-with #\# name)
                          (alexandria:ends-with #\b name))
        for text-color = (if black-p (color #xff #xff #xff #xc0) (color #x00 #x00 #x00 #xc0))
        for bg-color = (if black-p (color #x00 #x00 #x00 #xc0) (color #xff #xff #xff #xc0))
        for y-world = (key-to-world-y self key)
        for pos1 = (@ (.x window-pos) y-world)
        for pos2 = (@+ pos1 (@ (.offset-x self) key-height))
        do (ig:add-rect-filled draw-list pos1 pos2 bg-color)
           (ig:add-line draw-list pos1 (@+ pos1 (@ window-width .0)) (.color-line *theme*))
           (when (text-show-p self)
             (let ((pos-text (@ (ig:get-scroll-x) (key-to-local-y self key))))
               (when (and (<= .0 (.x pos-text)) (<= .0 (.y pos-text)))
                 (ig:set-cursor-pos pos-text)
                 (ig:with-button-color ((color 0 0 0 0))
                   (ig:push-style-color-u32 ig:+im-gui-col-text+ text-color)
                   (ig:button name)
                   (ig:pop-style-color 1)))))
        finally (progn                  ;height 確保のために
                  (ig:set-cursor-pos (@- pos2 window-pos))
                  (ig:text ""))))

(defmethod render-notes ((self piano-roll))
  (loop with clip = (.clip self)
        with draw-list = (ig:get-window-draw-list)
        with key-height = (.zoom-y self)
        with mouse-pos = (ig:get-mouse-pos)
        for note in (.notes (.seq clip))
        for x = (time-to-world-x self (.time note))
        for y = (key-to-world-y self (.key note))
        for pos1 = (@ x y)
        for pos2 = (@+ pos1 (@ (coerce (* (.duration note) (.zoom-x self)) 'single-float)
                               key-height))
        do (ig:with-id (note)
             (ig:add-rect-filled draw-list
                                 (@+ pos1 (@ .0 2.0))
                                 (@- pos2 (@ .0 1.0))
                                 (color-selected (.color note) (member note (.notes-selected self)))
                                 :rounding 2.5)
             (when (text-show-p self)
               (ig:add-text draw-list (@+ pos1 (@ 4.0 2.0)) (.color-text *theme*) (.name note)))
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
          (let* ((zoom-y (/ (- (ig:get-window-height) (.offset-y self))
                            (+ (- key-max key-min) 3.0))))
            (if (/= (.zoom-y self) zoom-y)
                (progn
                  (log:debug zoom-y)
                  (setf (.zoom-y self) zoom-y)
                  t)
                (let ((scroll-y (max .0
                                     (- (key-to-local-y self (1+ key-max)) (.offset-y self)))))
                  (log:debug scroll-y zoom-y)
                  (ig:set-scroll-y-float scroll-y)
                  nil))))
        (progn
          (ig:set-scroll-y-float (key-to-local-y self +c4+))
          nil))))

(defmethod world-pos-to-time-key ((self piano-roll) pos)
  (let* ((time (world-x-to-time self (.x pos)))
         (key (world-y-to-key self (.y pos))))
    (values time key)))

(defmethod world-x-to-time ((self piano-roll) x)
  (+ (/ (- x (.x (ig:get-window-pos)) (.offset-x self))
        (.zoom-x self))
     (ig:get-scroll-x)))

(defmethod world-y-to-key ((self piano-roll) y)
  (let ((local-y (+ (- y (.y (ig:get-window-pos)) (.offset-y self))
                    (ig:get-scroll-y))))
    (- 127 (floor (/ local-y (.zoom-y self))))))

(in-package :dgw)

(defmethod handle-mouse ((self piano-roll))
  (let* ((io (ig:get-io))
         (mouse-pos (ig:get-mouse-pos)))
    (cond ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
           (multiple-value-bind (time key) (world-pos-to-time-key self mouse-pos)
             (when (and (not (minusp time)) key)
               (cmd-add *project* 'cmd-note-add
                        :clip-id (.neko-id (.clip self)) :time time :key key)))))
    (zoom-x-update self io)
    (zoom-y-update self io)))

(defmethod key-to-local-y ((self piano-roll) key)
  (* (.zoom-y self) (- 128 key)))

(defmethod key-to-world-y ((self piano-roll) key)
  (+ (key-to-local-y self key)
     (.y (ig:get-window-pos))
     (- (ig:get-scroll-y))))

(defmethod render ((self piano-roll))
  (when (ig:begin "##piano-roll" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (ig:text (.name (.clip self)))
    (when (ig:begin-child "##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
      (render-time-ruler self)
      (render-keyboard self)
      (render-notes self)
      (swhen (.render-first-p self)
        (setf it (view-fit self)))
      (handle-mouse self)
      (ig:end-child))
    (ig:end)))

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
        for y-local = (key-to-local-y self key)
        for pos-local = (@ .0 y-local)
        for y-world = (key-to-world-y self key)
        for pos1 = (@ .0 y-world)
        for pos2 = (@+ pos1 (@ (.offset-x self) key-height))
        do (ig:add-rect-filled draw-list pos1 pos2 bg-color)
           (ig:add-line draw-list pos1 (@+ pos1 (@ window-width .0)) (.color-line *theme*))
           (when (text-show-p self)
             (when (and (<= .0 (.x pos-local)) (<= .0 (.y pos-local)))
               (ig:set-cursor-pos pos-local)
               (ig:with-button-color ((color 0 0 0 0))
                 (ig:push-style-color-u32 ig:+im-gui-col-text+ text-color)
                 (ig:button name)
                 (ig:pop-style-color 1))))
        finally (progn                  ;height 確保のために
                  (ig:set-cursor-pos (@- pos2 window-pos))
                  (ig:text ""))))

(defmethod render-notes ((self piano-roll))
  (loop with clip = (.clip self)
        with draw-list = (ig:get-window-draw-list)
        with key-height = (.zoom-y self)
        for note in (.notes (.seq clip))
        for x = (time-to-world-x self (.time note))
        for y = (key-to-world-y self (.key note))
        for pos1 = (@ x y)
        for pos2 = (@+ pos1 (@ (coerce (* (.duration note) (.zoom-x self)) 'single-float)
                               key-height))
        do (ig:with-id (note)
             (ig:add-rect-filled draw-list pos1 pos2 (.color note) :rounding 1.0)
             (when (text-show-p self)
               (ig:add-text draw-list (@+ pos1 (@ 4.0 2.0)) (.color-text *theme*) (.name note))))))

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
          (let* ((zoom-y (/ (- (ig:get-window-height) (.offset-x self))
                           (+ (- key-max key-min) 2.0))))
            (if (/= (.zoom-y self) zoom-y)
                (progn
                  (log:debug zoom-y)
                  (setf (.zoom-y self) zoom-y)
                  t)
                (let ((scroll-y (key-to-local-y self (floor (/ (- key-max key-min) 2)))))
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

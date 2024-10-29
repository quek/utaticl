(in-package :utaticl.core)

(defmethod playhead-y ((self time-ruler-mixin))
  (- (time-to-local-y self (.play-start (.project self)))
     (ig:get-scroll-y)))

(defmethod render-loop ((time-ruler-mixin time-ruler-mixin))
  (let ((project (.project time-ruler-mixin))
        (mouse-pos (ig:get-mouse-pos)))
    (labels ((in-p (pos)
               (let ((window-pos (ig:get-window-pos)))
                 (and (< (.x window-pos) (.x pos) (+ (.x window-pos) (.offset-x time-ruler-mixin)))
                      (< (+ (.y window-pos) (.offset-y time-ruler-mixin))
                         (.y pos)
                         (+ (.y window-pos) (ig:get-window-height))))))
             (time-at (pos)
               (time-grid-applied time-ruler-mixin
                                  (world-y-to-time time-ruler-mixin (.y pos)) #'round)))
      (cond ((.loop-selecting-p time-ruler-mixin)
             (let ((start (.loop-selecting-time time-ruler-mixin))
                   (end (time-at mouse-pos)))
               (if (< start end)
                   (progn
                     (setf (.loop-start project) start)
                     (setf (.loop-end project) end))
                   (progn
                     (setf (.loop-start project) end)
                     (setf (.loop-end project) start))))
             (when (ig:is-mouse-released ig:+im-gui-mouse-button-left+)
               (setf (.loop-selecting-p time-ruler-mixin) nil)))
            ((and (.loop-selecting-time time-ruler-mixin)
                  (in-p mouse-pos)
                  (ig:is-mouse-dragging ig:+im-gui-mouse-button-left+))
             (setf (.loop-selecting-p time-ruler-mixin) t))
            ((and (in-p mouse-pos)
                  (ig:is-mouse-clicked ig:+im-gui-mouse-button-left+))
             (setf (.loop-selecting-time time-ruler-mixin)
                   (time-at mouse-pos)))))
    (let* ((window-pos (ig:get-window-pos))
           (project project)
           (loop-start (.loop-start project))
           (loop-end (.loop-end project))
           (loop-start-y (time-to-world-y time-ruler-mixin loop-start))
           (loop-end-y (time-to-world-y time-ruler-mixin loop-end))
           (offset-x (.offset-x time-ruler-mixin))
           (width 10.0)
           (pos1 (@ (+ (.x window-pos) (- offset-x width)) loop-start-y))
           (pos2 (@ (+ (.x pos1) width) loop-end-y))
           (draw-list (ig:get-window-draw-list)))
      (ig:add-rect-filled draw-list pos1 pos2 (color #xff #xff #xff)))))

(defmethod render-playhead ((self time-ruler-mixin))
  (let* ((y (playhead-y self))
         (pos1 (@+ (@ .0 y) *window-pos*))
         (pos2 (@+ pos1 (@ (.x *window-size*) .0))))
    (ig:add-line *draw-list* pos1 pos2 (.color-playhead *theme*))))

(defun compute-time-y-delta (time-ruler-threshold zoom-y)
  (values-list
   (if (<= zoom-y time-ruler-threshold)
       (loop for time-delta = 1 then (* time-delta 2)
             for y-delta = (* zoom-y time-delta)
               thereis (and (<= time-ruler-threshold y-delta)
                            (list time-delta y-delta)))
       (loop for time-delta = 1 then (/ time-delta 2)
             for y-delta = (* zoom-y time-delta)
               thereis (and (> time-ruler-threshold y-delta)
                            (list (* time-delta 2)
                                  (* zoom-y (* time-delta 2))))))))

#+nil
(compute-time-y-delta 25.0 100.0)
;;⇒ 1/4
;;   25.0
#+nil
(compute-time-y-delta 25.0 0.5)
;;⇒ 64
;;   32.0

(defmethod render-time-ruler ((self time-ruler-mixin))
  (multiple-value-bind (time-delta y-delta)
      (compute-time-y-delta (.time-ruler-threshold self) (.zoom-y self))
    (loop with draw-list = (ig:get-window-draw-list)
          with window-pos = (ig:get-window-pos)
          with window-size = (ig:get-window-size)
          with scroll-x = (ig:get-scroll-x)
          with scroll-y = (ig:get-scroll-y)
          for time from 0 by time-delta
          for y from (.offset-y self) by y-delta
          for cursor-pos = (list scroll-x y)
          do (cond ((<= (+ scroll-y (.y window-size)) (.y cursor-pos))
                    (ig:set-cursor-pos (@+ cursor-pos (@ .0 100.0)))
                    (ig:text " ")
                    (loop-finish))
                   ((<= (+ (.offset-y self) scroll-y) (.y cursor-pos))
                    (when (zerop (mod (/ time 4) 1))
                      (ig:set-cursor-pos cursor-pos)
                      (ig:text (format nil " ~d" (1+ (/ time 4)))))
                    (let* ((p1 (@+ cursor-pos window-pos (@ (- scroll-x) (- scroll-y))))
                           (p2 (@+ p1 (@ (.x window-size) 0.0))))
                      (when (<= (+ (.offset-y self) (.y window-pos)) (.y p1))
                        (ig:add-line draw-list p1 p2
                                     (if (zerop (mod (/ time 4) 1))
                                         (.color-line *theme*)
                                         (.color-line-sub *theme*)))))))))
  (render-loop self)
  (render-playhead self))

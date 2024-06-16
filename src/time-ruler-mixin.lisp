(in-package :dgw)

(defmethod render-playhead ((self time-ruler-mixin))
  (let* ((draw-list (ig:get-window-draw-list))
         (window-pos (ig:get-window-pos))
         (y (time-to-local-y self (.play-start *project*)))
         (pos1 (@+ (@ .0 y) window-pos))
         (pos2 (@+ pos1 (@ (ig:get-window-width) .0))))
    (ig:add-line draw-list pos1 pos2 (.color-playhead *theme*))))

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

  (render-playhead self))

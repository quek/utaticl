(in-package :dgw)

(defmethod render-playhead ((self time-ruler-mixin))
  (let* ((draw-list (ig:get-window-draw-list))
         (window-pos (ig:get-window-pos))
         (x (time-to-local-x self (.play-start *project*)))
         (pos1 (@+ (@ x .0) window-pos))
         (pos2 (@+ pos1 (@ .0 (ig:get-window-height)))))
    (ig:add-line draw-list pos1 pos2 (.color-playhead *theme*))))

(defun compute-time-x-delta (time-ruler-threshold zoom-x)
  (values-list
   (if (<= zoom-x time-ruler-threshold)
       (loop for time-delta = 1 then (* time-delta 2)
             for x-delta = (* zoom-x time-delta)
               thereis (and (<= time-ruler-threshold x-delta)
                            (list time-delta x-delta)))
       (loop for time-delta = 1 then (/ time-delta 2)
             for x-delta = (* zoom-x time-delta)
               thereis (and (> time-ruler-threshold x-delta)
                            (list (* time-delta 2)
                                  (* zoom-x (* time-delta 2))))))))

#+nil
(compute-time-x-delta 25.0 100.0)
;;⇒ 1/4
;;   25.0
#+nil
(compute-time-x-delta 25.0 0.5)
;;⇒ 64
;;   32.0

(defmethod render-time-ruler ((self time-ruler-mixin))
  (multiple-value-bind (time-delta x-delta)
      (compute-time-x-delta (.time-ruler-threshold self) (.zoom-x self))
    (loop with draw-list = (ig:get-window-draw-list)
          with window-pos = (ig:get-window-pos)
          with window-size = (ig:get-window-size)
          with scroll-x = (ig:get-scroll-x)
          with scroll-y = (ig:get-scroll-y)
          for time = 0 then (+ time time-delta)
          for x = (.offset-x self) then (+ x x-delta)
          for cursor-pos = (list x scroll-y)
          do (cond ((<= (+ scroll-x (.x window-size)) (.x cursor-pos))
                    (ig:set-cursor-pos (@+ cursor-pos (@ 100.0 .0)))
                    (ig:text " ")
                    (loop-finish))
                   ((<= (+ (.offset-x self) scroll-x) (.x cursor-pos))
                    (when (zerop (mod (/ time 4) 1))
                      (ig:set-cursor-pos cursor-pos)
                      (ig:text (format nil " ~d" (1+ (/ time 4)))))
                    (let* ((p1 (@+ cursor-pos window-pos (@ (- scroll-x) (- scroll-y))))
                           (p2 (@+ p1 (@ 0.0 (.y window-size)))))
                      (when (<= (+ (.offset-x self) (.x window-pos)) (.x p1))
                        (ig:add-line draw-list p1 p2 (.color-line *theme*))))))))

  (render-playhead self))

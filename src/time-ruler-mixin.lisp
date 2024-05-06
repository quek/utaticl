(in-package :dgw)

(defmethod max-bar ((self time-ruler-mixin))
  ;; TODO
  8)

(defmethod render-playhead ((self time-ruler-mixin))
  (let* ((draw-list (ig:get-window-draw-list))
         (window-pos (ig:get-window-pos))
         (x (time-to-local-x self (.play-start *project*)))
         (pos1 (@+ (@ x .0) window-pos))
         (pos2 (@+ pos1 (@ .0 (ig:get-window-height)))))
    (ig:add-line draw-list pos1 pos2 (.color-playhead *theme*))))

(defmethod render-time-ruler ((self time-ruler-mixin))
  (let* ((draw-list (ig:get-window-draw-list))
         (max-bar (max-bar self))
         (window-pos (ig:get-window-pos))
         (window-size (ig:get-window-size))
         (scroll-x (ig:get-scroll-x))
         (scroll-y (ig:get-scroll-y)))
    (loop for bar from 0 to max-bar
          for x = (+ (* bar 4 (.zoom-x self))
                     (.offset-x self))
          for cursor-pos = (list x scroll-y)
          do (when (<= (+ (.offset-x self) scroll-x) (.x cursor-pos))
               (ig:set-cursor-pos cursor-pos)
               (ig:text (format nil " ~d" (1+ bar)))
               (let* ((p1 (@+ cursor-pos window-pos (@ (- scroll-x) (- scroll-y))))
                      (p2 (@+ p1 (@ 0.0 (.y window-size)))))
                 (when (<= (+ (.offset-x self) (.x window-pos)) (.x p1))
                   (ig:add-line draw-list p1 p2 (.color-line *theme*))))))

    (render-playhead self)))

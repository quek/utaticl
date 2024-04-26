(in-package :dgw)

(defmethod render ((self arrangement))
  (when (ig:begin "##arrangement")
    (ig:text "Arrangement")
    (when (ig:begin-child "##canvas")
      (render-time-ruler self)
      (render (.master-track *project*))
      (ig:text "child"))
    (ig:end-child))
  (ig:end))


(defmethod render-time-ruler ((self arrangement))
  (let* ((draw-list (ig:get-window-draw-list))
         (max-bar (max-bar self))
         (window-pos (ig:get-window-pos))
         (window-size (ig:get-window-size))
         (scroll-x (ig:get-scroll-x)))
    (loop for bar from 0 to max-bar
          for x = (+ (* bar 4 (.zoom-x self))
                     (.track-width self)
                     scroll-x)
          for cursor-pos = (list x 0.0)
          do (ig:set-cursor-pos cursor-pos)
             (ig:text (format nil "~d" (1+ bar)))
             (let* ((p1 (@+ cursor-pos window-pos))
                    (p2 (@+ p1 (@ 0.0 (.y window-size)))))
               (ig:add-line draw-list p1 p2 (color 0 0 #xff))))))

(defmethod max-bar ((self arrangement))
  ;; TODO
  8)

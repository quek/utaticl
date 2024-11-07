(in-package :utaticl.core)

(defmethod change ((self peak-meter) &rest values)
  (loop for svalue in values
        for value = (abs svalue)
        for i from 0
        for peak-value = (nth i (.values self))
        do (incf (nth i (.avgs-tmp self)) value)
        if (< peak-value value)
          do (setf (nth i (.values self)) value)
             (setf (nth i (.ats self)) (get-internal-real-time))))

(defmethod process-before ((self peak-meter))
  (loop for i below 2
        do (setf (nth i (.avgs-tmp self)) 0.0)))

(defmethod process-after ((self peak-meter))
  (loop for i from 0
        for avg in (.avgs self)
        for $avg-tmp in (.avgs-tmp self)
        for avg-tmp = (/ $avg-tmp (.frames-per-buffer *config*))
        do (setf (nth i (.avgs self))
                 (if (< avg avg-tmp)
                     avg-tmp
                     (+ (* avg 0.9)
                        (* avg-tmp 0.2))))))

(defmethod render-in ((self peak-meter) (rack rack) &key fader)
  (let* ((size (@ 21.0 (- (.y *window-size*) *scrollbar-size*
                          (* 2 (plus-c:c-ref *style* ig:im-gui-style :item-spacing :y)))))
         (cursor-pos (ig:get-cursor-pos))
         (pos1 (local-to-world rack cursor-pos))
         (pos2 (@+ pos1 size)))
    (ig:dummy size)
    (ig:add-rect-filled *draw-list* pos1 pos2 (color #x00 #x00 #x00 #x80))
    (loop for value in (.values self)
          for at in (.ats self)
          for avg in (.avgs self)
          for i from 0
          for peak-db = (to-db-float value)
          for peak-normalized = (%peak-meter-db-to-normalized peak-db)
          for avg-db = (to-db-float avg)
          for avg-normalized = (%peak-meter-db-to-normalized avg-db)
          unless (zerop avg-normalized)
            do (let* ((p1 (@+ pos1 (@ (+ (* (/ (.x size) 2) i) (* 1.0 i))
                                      (* (.y size) (- 1.0 avg-normalized)))))
                      (p2 (@ (+ (.x p1) (/ (.x size) 2))
                             (+ (.y pos1) (.y size)))))
                 (ig:add-rect-filled *draw-list* p1 p2 (color #x00 #xff #x00 #x80)))
          unless (zerop peak-normalized)
            do (let* ((p1 (@+ pos1 (@ (+ (* (/ (.x size) 2) i) (* 1.0 i))
                                      (* (.y size) (- 1.0 peak-normalized)))))
                      (p2 (@+ p1 (@ (/ (.x size) 2) 3.0)))
                      (color (if (<= 0.0 peak-db)
                                 (color #xff #x00 #x00 #xcc)
                                 (color #xff #xff #x00 #xcc))))
                 (ig:add-rect-filled *draw-list* p1 p2 color))
          if (< (* internal-time-units-per-second 1)
                (- (get-internal-real-time) at))
            do (setf (nth i (.values self))
                     ;; こっちはリニアに下がった方がよさそう
                     (max 0.0 (- value 0.003))))
    (loop for db in '(6 0 -6 -12 -18 -24 -32 -50)
          for db-normalized = (%peak-meter-db-to-normalized db)
          for y = (* (.y size) (- 1.0 db-normalized))
          for pos = (@+ cursor-pos
                        (@ 0.0 y))
          for p1 = (@+ pos1 (@ 0.0 y))
          for p2 = (@+ p1 (@ (.x size) 0.0))
          do (ig:set-cursor-pos pos)
             (ig:add-line *draw-list* p1 p2 (color #xff #xff #xff))
             (ig:text (format nil "~3d" db)))

    ;; おためし
    (ig:set-cursor-pos (@+ cursor-pos (@ 25.0 0.0)))
    (ig:v-slider-scalar "##"
                        (@ 20.0 (.y size))
                        ig:+im-gui-data-type-double+
                        (.value (param fader 'volume))
                        0.0d0 1.0d0)
    (ig:set-cursor-pos (@+ cursor-pos (@ 50.0 0.0)))
    (ig:text (format nil "~,3f ~,3f ~,3f"
                     (* (expt (.value (param fader 'volume))
                              3.10628)
                        2.0)
                     (expt (* (.value (param fader 'volume))
                              6.5)
                           7.0)
                     (%peak-meter-db-to-normalized (to-db (.value (param fader 'volume))))))
    ;; 以下デバッグ
    (ig:set-cursor-pos (@+ cursor-pos (@ 50.0 20.0)))
    (ig:text (format nil "~,3f ~,3f ~,3f"
                     (car (.values self))
                     (to-db-float (car (.values self)))
                     (%peak-meter-db-to-normalized (to-db-float (car (.values self))))))
    (ig:set-cursor-pos (@+ cursor-pos (@ 50.0 40.0)))
    (ig:text (format nil "~,3f ~,3f ~,3f"
                     (car (.avgs self))
                     (to-db-float (car (.avgs self)))
                     (%peak-meter-db-to-normalized (to-db-float (car (.values self))))))
    ))

(defun %peak-meter-db-to-normalized (db)
  "最大が 6db 最小が -180db"
  (let ((normalized (expt (/ (- db +min-db-float+)
                             (- 6.5     ;最大 6db + 余白 0.5
                                +min-db-float+))
                          7.0)))        ;適当なメモリの間隔
    (min 1.0 (max 0.0 normalized))))



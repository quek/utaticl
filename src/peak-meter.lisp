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

(defmethod render-in ((self peak-meter) (rack rack) &key)
  (let* ((size (@ 21.0 (- (.y *window-size*) *scrollbar-size*
                          (* 2 (plus-c:c-ref *style* ig:im-gui-style :item-spacing :y)))))
         (cursor-pos (ig:get-cursor-pos))
         (pos1 (local-to-world rack cursor-pos))
         (pos2 (@+ pos1 size)))
    (ig:dummy size)
    (ig:add-rect-filled *draw-list* pos1 pos2 (color #x03 #x03 #x03 #x80))
    (loop for value in (.values self)
          for at in (.ats self)
          for avg in (.avgs self)
          for i from 0
          for peak-normalized = (%peak-meter-db-to-normalized (to-db-float value))
          for avg-normalized = (%peak-meter-db-to-normalized (to-db-float avg))
          do (let* ((p1 (@+ pos1 (@ (+ (* (/ (.x size) 2) i) (* 1.0 i))
                                    (* (.y size) (- 1.0 avg-normalized)))))
                    (p2 (@+ p1 (@ (/ (.x size) 2) (.y size)))))
               (ig:add-rect-filled *draw-list* p1 p2 (color #x00 #xff #x00 #x80)))
             (let* ((p1 (@+ pos1 (@ (+ (* (/ (.x size) 2) i) (* 1.0 i))
                                    (* (.y size) (- 1.0 peak-normalized)))))
                    (p2 (@+ p1 (@ (/ (.x size) 2) 3.0))))
               (ig:add-rect-filled *draw-list* p1 p2 (color #xff #xff #x00 #xcc)))
          if (< (* internal-time-units-per-second 1)
                (- (get-internal-real-time) at))
            do (setf (nth i (.values self))
                     ;; こっちはリニアに下がった方がよさそう
                     (max 0.0 (- value 0.003))))
    (loop for db in '(6 0 -6 -12 -18 -32 -64)
          for pos = (@+ cursor-pos
                        (@ 0.0
                           (* (.y size)
                              (- 1.0 (%peak-meter-db-to-normalized db)))))
          do (ig:set-cursor-pos pos)
             (ig:text (format nil "~d" db)))))

(defun %peak-meter-db-to-normalized (db)
  "最大が 6db 最小が -180db"
  (let ((normalized (expt (/ (- db -180.0)
                             (- 6.0 -180.0))
                          7.0)))
    (if (< normalized 0.0)
        0.0
        (if (> normalized 1.0)
            1.0
            normalized))))

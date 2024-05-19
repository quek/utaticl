(in-package :dgw)

(defmethod clip-add ((self lane) clip)
  (setf (.clips self)
        (sort (cons clip (.clips self))
              (lambda (x y)
                (< (.time x) (.time y))))))

(defmethod clip-delete ((self lane) clip)
  (setf (.clips self)
        (delete clip (.clips self))))

(defmethod diff ((a lane) (b lane))
  (if (eq a b)
      0
      (let ((diff nil))
        (labels ((f (track)
                   (loop for lane in (.lanes track)
                         if (or (eq lane a)
                                (eq lane b))
                           do (if (and diff (plusp diff))
                                  (return-from diff diff)
                                  (setf diff 1))
                         else
                           do (and diff (incf diff)))
                   (loop for x in (.tracks track)
                         do (f x))))
          (f (.master-track *project*)))
        diff)))

(defmethod prepare-event ((self lane) start end loop-p offset-samples)
  (loop for clip in (.clips self)
        for clip-start = (.time clip)
        for clip-end = (+ clip-start (.duration clip))
        if (and (< clip-start end)
                (< start clip-end))
          do (prepare-event clip start end loop-p offset-samples)))

(defmethod relative-at ((self lane) delta)
  (if (zerop delta)
      self
      (let* ((all (lane-all *project*))
             (pos (position self all)))
        (nth (+ pos delta) all))))

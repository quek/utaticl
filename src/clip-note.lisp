(in-package :dgw)

;; TODO これいらないよね
;; (defmethod initialize-instance :after ((self clip-note) &key seq)
;;   (setf (.seq self) (or seq (make-instance 'seq-note))))

(defmethod edit ((self clip-note))
  (setf (.piano-roll (.project self))
        (make-instance 'piano-roll :clip self)))

(defmethod note-add ((self clip-note) (note note))
  (note-add (.seq self) note))

(defmethod note-delete ((self clip-note) (note note))
  (note-delete (.seq self) note))

(defmethod .notes ((self clip-note))
  (.notes (.seq self)))

(defmethod prepare-event :around ((clip-note clip-note) start end loop-p offset-samples)
  (if (and (.sceen clip-note)
           (= .0d0 start))
      (cond ((.will-start clip-note)
             (setf (.play-p clip-note) t)
             (setf (.will-start clip-note) nil))
            ((.will-stop clip-note)
             (setf (.play-p clip-note) nil)
             (setf (.will-stop clip-note) nil))
            ((.clip-next clip-note)
             (setf (.play-p clip-note) nil)
             (setf (.play-p (.clip-next clip-note)) t)
             (setf (.clip-next clip-note) nil))
            (t (call-next-method)))
      (unless (.will-start clip-note)
        (call-next-method))))

(defmethod prepare-event ((self clip-note) start end loop-p offset-samples)
  (loop for note in (.notes self)
        for note-start = (+ (.time note) (.time self))
        for note-end = (+ note-start (.duration note))
        do (cond ((and (<= start note-start)
                       (< note-start end))
                  (note-on *process-data*
                           (.key note)
                           (.channel note)
                           (.velocity note)
                           (+ offset-samples (time-to-sample (.project self) (- note-start start)))))
                 ((and (< start note-end)
                       (<= note-end end))
                  (note-off *process-data*
                            (.key note)
                            (.channel note)
                            1.0
                            (+ offset-samples (time-to-sample (.project self) (- note-end start)))))
                 ((and loop-p
                       (< note-start end)
                       (<= end note-end))
                  ;; ノートの途中でループの折り返しなので note off する
                  (note-off *process-data*
                            (.key note)
                            (.channel note)
                            1.0
                            (+ offset-samples (time-to-sample (.project self) (- end start)))))
                 ((<= end note-start)
                  ;; note は time 順にソートされている
                  (loop-finish)))))

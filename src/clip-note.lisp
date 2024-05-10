(in-package :dgw)

(defmethod edit ((self clip-note))
  (setf (.piano-roll *project*)
        (make-instance 'piano-roll :clip self)))

(defmethod note-add ((self clip-note) (note note))
  (note-add (.seq self) note))

(defmethod note-add ((self seq-note) (note note))
  (setf (.notes self)
        (sort (cons note (.notes self))
              (lambda (x y) (< (.time x) (.time y))))))

(defmethod .notes ((self clip-note))
  (.notes (.seq self)))

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
                           (+ offset-samples (time-to-sample *project* (- note-start start)))))
                 ((and (< start note-end)
                       (<= note-end end))
                  (note-off *process-data*
                            (.key note)
                            (.channel note)
                            1.0
                            (+ offset-samples (time-to-sample *project* (- note-end start)))))
                 ((and loop-p
                       (< note-start end)
                       (<= end note-end))
                  ;; ノートの途中でループの折り返しなので note off する
                  (note-off *process-data*
                            (.key note)
                            (.channel note)
                            1.0
                            (+ offset-samples (time-to-sample *project* (- end start)))))
                 ((<= end note-start)
                  ;; note は time 順にソートされている
                  (loop-finish)))))

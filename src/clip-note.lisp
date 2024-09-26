(in-package :utaticl.core)

;; TODO これいらないよね
;; (defmethod initialize-instance :after ((self clip-note) &key seq)
;;   (setf (.seq self) (or seq (make-instance 'seq-note))))

(defmethod range-copy ((clip-note clip-note) range)
  (loop for note in (.notes clip-note)
        with (time1 key1 time2 key2) = range
        if (and (< time1 (time-end note))
                (< (.time note) time2)
                (<= key1 (.key note) key2))
          collect (let ((note (copy note)))
                    (when (< (.time note) time1)
                      (let ((delta (- time1 (.time note))))
                        (decf (.duration note) delta))
                      (setf (.time note) time1))
                    (let ((time-end (+ (.time note) (.duration note))))
                      (when (< time2 time-end)
                        (decf (.duration note) (- time-end time2))))
                    (note-add clip-note note)
                    note)))

(defmethod edit ((self clip-note) clips)
  (setf (.piano-roll (.project self))
        (make-instance 'piano-roll :clip self :clips clips)))

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
                           note
                           (+ offset-samples (time-to-sample (.project self) (- note-start start)))))
                 ((and (< start note-end)
                       (<= note-end end))
                  (note-off *process-data*
                            note
                            (+ offset-samples (time-to-sample (.project self) (- note-end start)))))
                 ((and loop-p
                       (< note-start end)
                       (<= end note-end))
                  ;; ノートの途中でループの折り返しなので note off する
                  (note-off *process-data*
                            note
                            (+ offset-samples (time-to-sample (.project self) (- end start)))))
                 ((<= end note-start)
                  ;; note は time 順にソートされている
                  (loop-finish)))))

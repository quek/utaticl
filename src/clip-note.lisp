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

(defmethod prepare-event ((self clip-note) start end loop-p)
  (loop for note in (.notes self)
        for note-start = (+ (.time note) (.time self))
        for note-end = (+ note-start (.duration note))
        do (cond ((and (<= start note-start)
                       (< note-start end))
                  ;; TODO note on
                  )
                 ((and (< start note-end)
                       (<= note-end end))
                  ;; TODO note off
                  )
                 ((and loop-p
                       (< note-start end)
                       (<= end note-end))
                  ;; TODO note off
                  ))))

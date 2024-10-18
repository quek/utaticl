(in-package :utaticl.core)

(defmethod .automation-param ((self clip-automation))
  (.automation-param (.lane self)))

(defmethod edit ((self clip-automation) clips)
  (setf (.editor-automation *project*)
        (make-instance 'editor-automation :clip self)))

(defmethod .points ((self clip-automation))
  (.points (.seq self)))

(defmethod prepare-event ((self clip-automation) start end loop-p offset-samples)
  (let* ((clip-time (.time self))
         (start (max (- start clip-time) .0d0))
         (end (- end clip-time))
         (seq (.seq self)))
    (loop for (point point-next) on (.points seq)
          for point-prev = nil then point
          for point-start = (.time point)
          if (and (<= start point-start)
                  (< point-start end))
            do (let* ((param (.automation-param self))
                      (module (.module param))
                      (param-id (.id param))
                      (frame-rate (* (/ 60 (.bpm *project*)) (.sample-rate *config*)))
                      (time-prev (if point-prev (.time point-prev) start))
                      (time-prev-frame (round (* time-prev frame-rate)))
                      (value-prev (if point-prev (.value point-prev) (.value point)))
                      (time-next (if point-next (.time point-next) end))
                      (time-next-frame (round (* time-next frame-rate)))
                      (value-next (if point-next (.value point-next) (.value point)))
                      (delta (/ (- value-next value-prev)
                                (- time-next-frame time-prev-frame)))
                      (start-frame (round (* start frame-rate)))
                      (end-frame (round (* end frame-rate))))

                 (cond ((and (null point-prev) (null point-next))
                        ;; ポイント一つで値の変化なし
                        (setf (.value param) (.value point))
                        (param-change-add module param-id (.value param)))
                       (t
                        (loop for i from start-frame below end-frame
                              for value = (+ (* (- i start-frame) delta)
                                             value-prev)
                              do (param-change-add module param-id value
                                                   (- i start-frame))
                              finally (setf (.value param) value)))))

               (loop-finish))))

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
         (seq (.seq self))
         (param (.automation-param self))
         (module (.module param))
         (param-id (.id param)))
    (cond ((null (.points seq)))
          ((= 1 (length (.points seq)))
           (param-change-add module param-id
                             (setf (.value param)
                                   (.value (car (.points seq)))))
           (value-changed-by-host-without-process param))
          (t
           (let ((point-prev nil)
                 (point-current nil)
                 (point-next nil))
             (loop for point in (.points seq)
                   for time = (.time point)
                   do (cond ((and (< time start)
                                  (or (null point-prev)
                                      (< (.time point-prev) time)))
                             (setf point-prev point))
                            ((and (<= start time)
                                  (< time end))
                             (setf point-current point))
                            ((<= end time)
                             (setf point-next point)
                             (loop-finish))))
             ;(break "~a ~a ~a" point-prev point-current point-next)
             (cond (point-current
                    (%clip-automation-prepare-event
                     self
                     (if point-prev (.time point-prev) start)
                     (if point-prev (.value point-prev) (.value point-current))
                     (.time point-current)
                     (.value point-current)
                     start (.time point-current) 0)
                    (%clip-automation-prepare-event
                     self
                     (.time point-current)
                     (.value point-current)
                     (if point-next (.time point-next) (.time point-current))
                     (if point-next (.value point-next) (.value point-current))
                     (.time point-current) end
                     (- end (.time point-current))))
                   ((and point-prev point-next)
                    (%clip-automation-prepare-event
                     self
                     (.time point-prev)
                     (.value point-prev)
                     (.time point-next)
                     (.value point-next)
                     start end 0))
                   (point-prev
                    (%clip-automation-prepare-event
                     self
                     (.time point-prev)
                     (.value point-prev)
                     end
                     (.value point-prev)
                     start end 0))
                   (point-next
                    (%clip-automation-prepare-event
                     self
                     start
                     (.value point-next)
                     (.time point-next)
                     (.value point-next)
                     start end 0))
                   (t
                    (error "bug!"))))
           (value-changed-by-host-without-process param)))))

(defun %clip-automation-prepare-event (self time1 value1 time2 value2 start end time-offset)
  (let* ((frame-rate (* (/ 60 (.bpm *project*)) (.sample-rate *config*)))
         (param (.automation-param self))
         (module (.module param))
         (param-id (.id param))
         (time1-frame (round (* time1 frame-rate)))
         (time2-frame (round (* time2 frame-rate)))
         (delta (/ (- value2 value1)
                   (- time2-frame time1-frame)))
         (start-frame (round (* start frame-rate)))
         (end-frame (round (* end frame-rate))))

    (if (zerop delta)
        (progn
          ;; 値の変化なし
          (setf (.value param) value1)
          (param-change-add module param-id (.value param)))
        (progn
          (loop for i from start-frame below end-frame
                for value-n-1 = -1d0 then value
                for value = (+ (* (- i start-frame) delta)
                               value1)
                if (/= value value-n-1)
                  do (param-change-add module param-id value
                                       (round (+ (- i start-frame)
                                                 (* time-offset frame-rate))))
                finally (setf (.value param) value))))))

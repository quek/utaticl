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
                     (+ (* avg 0.98)
                        (* avg-tmp 0.02))))))

(defmethod render ((self peak-meter))
  (loop for value in (.values self)
        for at in (.ats self)
        for label in '("L" "R")
        for avg in (.avgs self)
        for i from 0
        do (ig:slider-float label value 0.0 1.0)
           (ig:slider-float label avg 0.0 1.0)
        if (< (* internal-time-units-per-second 1)
              (- (get-internal-real-time) at))
          do (setf (nth i (.values self))
                   ;; こっちはリニアに下がった方がよさそう
                   (max 0.0 (- value 0.003)))))

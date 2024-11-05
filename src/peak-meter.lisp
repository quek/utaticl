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
        for avg-tmp in (.avgs-tmp self)
        do (setf (nth i (.avgs self))
                 (+ (* (nth i (.avgs self)) 0.8)
                    (* (/ avg-tmp (.frames-per-buffer *config*)) 0.2)))))

(defmethod render ((self peak-meter))
  (loop for value in (.values self)
        for at in (.ats self)
        for label in '("L" "R")
        for avg in (.avgs self)
        for i from 0
        do (ig:slider-float label value 0.0 1.0)
           (ig:slider-float label avg 0.0 1.0)
           (ig:text (format nil "~,3f ~a"
                            value
                            at))
        if (< (* internal-time-units-per-second 1)
              (- (get-internal-real-time) at))
          do (setf (nth i (.values self)) (max 0.0 (- value 0.003)))))

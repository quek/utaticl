(in-package :utaticl.core)

(defmethod change ((self peak-meter) &rest values)
  (loop for value in values
        for i from 0
        for peak-value = (nth i (.values self))
        if (< peak-value value)
          do (setf (nth i (.values self)) value)
             (setf (nth i (.ats self)) (get-internal-real-time))))

(defmethod render ((self peak-meter))
  (loop for value in (.values self)
        for at in (.ats self)
        for i from 0
        do (ig:slider-float "L" value 0.0 1.0)
           (ig:text (format nil "~,3f ~a"
                            value
                            at))
        if (< (* internal-time-units-per-second 1)
              (- (get-internal-real-time) at))
          do (setf (nth i (.values self)) (max 0.0 (- value 0.003)))))

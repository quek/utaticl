(in-package :utaticl.core)

(defmethod change ((self peak-meter) &rest values)
  (loop for value in values
        for i from 0
        for peak-value = (nth i (.values self))
        for peak-at = (nth i (.ats self))
        if (< peak-value value)
          do (setf (nth i (.values self)) value)
             (setf (nth i (.ats self)) (get-internal-real-time))
        if (< (* internal-time-units-per-second 2)
              (- (get-internal-real-time) peak-at))
          do (setf (nth i (.values self)) (max 0.0 (- peak-value 0.01)))))

(defmethod render ((self peak-meter))
  (loop for value in (.values self)
        for at in (.ats self)
        do (ig:text (format nil "~,3f ~a"
                            value
                            at))))

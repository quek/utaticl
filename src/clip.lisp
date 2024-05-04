(in-package :dgw)

(defmethod edit ((self clip-note))
  (setf (.piano-roll *project*)
        (make-instance 'piano-roll :clip self)))

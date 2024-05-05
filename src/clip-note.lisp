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

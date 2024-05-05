(in-package :dgw)

(defmethod edit ((self clip-note))
  (setf (.piano-roll *project*)
        (make-instance 'piano-roll :clip self)))

(defmethod note-add ((self clip-note) (note note))
  (note-add (.sequence-note self) note))

(defmethod note-add ((self sequence-note) (note note))
  (setf (.notes self)
        (sort (cons note (.notes self))
              (lambda (x y) (< (.time x) (.time y))))))

(defmethod note-delete ((self clip-note) (note note))
  (note-delete (.sequence-note self) note))

(defmethod note-delete ((self sequence-note) (note note))
  (setf (.notes self)
        (delete note (.notes self))))

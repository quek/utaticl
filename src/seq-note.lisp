(in-package :dgw)

(defmethod note-add ((self seq-note) (note note))
  (setf (.seq-note note) self)
  (setf (.notes self)
        (sort (cons note (.notes self))
              (lambda (x y) (< (.time x) (.time y))))))

(defmethod note-delete ((self seq-note) (note note))
  (setf (.seq-note note) nil)
  (setf (.notes self)
        (delete note (.notes self))))

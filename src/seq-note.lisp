(in-package :utaticl.core)

(defmethod initialize-instance :after ((self seq-note) &key)
  (when (string= "" (.name self))
    (setf (.name self) (name-new 'seq-note "N"))))

(defmethod note-add ((self seq-note) (note note))
  (setf (.seq-note note) self)
  (setf (.notes self)
        (sort (cons note (copy-list (.notes self)))
              (lambda (x y) (< (.time x) (.time y))))))

(defmethod note-delete ((self seq-note) (note note))
  (setf (.seq-note note) nil)
  (setf (.notes self)
        (delete note (.notes self))))

(in-package :dgw)

(defmethod note-delete ((self clip-note) (note note))
  (note-delete (.seq self) note))

(defmethod note-delete ((self seq-note) (note note))
  (setf (.notes self)
        (delete note (.notes self))))

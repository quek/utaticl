(in-package :dgw)

(defclass neko ()
  ((name :initarg :name :initform "" :accessor .name)
   (color :initarg :color :initform nil :accessor .color)))

(in-package :dgw)

(defvar *theme*)

(defclass theme ()
  ((color-line :initform (color #xff #xff #xff) :accessor .color-line)))

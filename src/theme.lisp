(in-package :dgw)

(defvar *theme*)

(defclass theme ()
  ((color-line :initform (color #xff #xff #xff) :accessor .color-line)
   (color-text :initform (color #xc0 #xc0 #xc0 #xff) :accessor .color-text)))

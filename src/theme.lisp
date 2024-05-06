(in-package :dgw)

(defvar *theme*)

(defclass theme ()
  ((color-button-toggle-on :initform (color #x60 #x60 #xff #x80) :accessor .color-button-toggle-on)
   (color-button-toggle-off :initform (color #x20 #x20 #xc0 #x80) :accessor .color-button-toggle-off)
   (color-line :initform (color #xff #xff #xff) :accessor .color-line)
   (color-text :initform (color #xc0 #xc0 #xc0 #xff) :accessor .color-text)))

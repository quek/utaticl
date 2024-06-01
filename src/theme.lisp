(in-package :dgw)

(defvar *theme*)

(defclass theme (config-mixin)
  ((color-button-toggle-on :initform (color #x20 #x20 #xff #xff) :accessor .color-button-toggle-on)
   (color-button-toggle-off :initform (color #x20 #x20 #x80 #xc0) :accessor .color-button-toggle-off)
   (color-line :initform (color #xff #xff #xff) :accessor .color-line)
   (color-playhead :initform (color #x40 #xff #x40 #xff) :accessor .color-playhead)
   (color-selected-region :initform (color #xcc #xcc #xcc #x60) :accessor .color-selected-region)
   (color-text :initform (color #xc0 #xc0 #xc0 #xff) :accessor .color-text))
  (:default-initargs :name "theme.lisp"))

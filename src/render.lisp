(in-package :dgw)

(defclass render-context ()
  ())

(defgeneric render (self context))

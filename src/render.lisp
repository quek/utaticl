(in-package :dgw)

(defvar *render-context*)

(defclass render-context ()
  ())

(defgeneric render (self))

(defmethod render ((self null)))


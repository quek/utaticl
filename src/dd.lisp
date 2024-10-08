(in-package :utaticl.core)

(defmethod dd-show (x)
  (ig:text (princ-to-string x)))

(defmethod dd-start ((self dd) (src list) &optional at)
  (setf (.at self) at)
  (setf (.src self) src))

(defmethod dd-start ((self dd) src &optional at)
  (dd-start self (list src) at))

(defmethod dd-start-p ((self dd))
  (.src self))

(defmethod dd-reset ((self dd))
  (setf (.at self) nil)
  (setf (.src self) nil))

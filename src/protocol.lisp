(in-package :dgw)

(defgeneric cmd-add (project cmd-class &rest args)
  (:method ((project null) cmd-class &rest args)
    (declare (ignore args))))

(defgeneric include-p (x y)
  (:method (x y) (in-p y x))
  (:method ((x null) y) nil))

(defgeneric in-p (x y)
  (:method (x y) (include-p y x)))

(defgeneric .parent (x)
  (:method ((x null))
    nil))

(defgeneric .project (x)
  (:method ((x null))))

(defgeneric render (x)
  (:method ((x null))))

(in-package :dgw)

(defgeneric cmd-add (project cmd-class &rest args)
  (:method ((project null) cmd-class &rest args)
    (declare (ignore args))))

(defgeneric .parent (x)
  (:method ((x null))
    nil))

(defgeneric .project (x)
  (:method ((x null))))

(defgeneric render (x)
  (:method ((x null))))

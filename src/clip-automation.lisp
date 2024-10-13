(in-package :utaticl.core)

(defmethod edit ((self clip-automation) clips)
  (setf (.editor-automation *project*)
        (make-instance 'editor-automation :clip self)))

(in-package :dgw)

(defmethod param-add ((self module-builtin) symbol name value)
  (let ((param (make-instance 'param :name name :value value)))
    (setf (gethash symbol (.params self)) param)))

(defmethod param ((self module-builtin) symbol)
  (gethash symbol (.params self)))


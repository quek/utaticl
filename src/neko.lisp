(in-package :dgw)

(defvar *neko-map* (make-hash-table :weakness :value))

(defun find-neko (neko-id)
  (gethash neko-id *neko-map*))

(defmethod initialize-instance :after ((self neko) &key)
  (setf (gethash (.neko-id self) *neko-map*) self))

(defmethod ig:push-id ((self neko))
  (ig:push-id (.neko-id self)))

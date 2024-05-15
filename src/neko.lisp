(in-package :dgw)

(defvar *neko-map* (make-hash-table :weakness :value :test 'equal))

(defun find-neko (neko-id)
  (gethash neko-id *neko-map*))

(defmethod initialize-instance :after ((self neko) &key)
  (setf (gethash (.neko-id self) *neko-map*) self))

(defmethod (setf .neko-id) :around (value (self neko))
  (let ((neko-id-old (.neko-id self)))
    (prog1 (call-next-method)
      (setf (gethash value *neko-map*) self)
      (remhash neko-id-old *neko-map*))))

(defmethod .neko-id ((self list))
  (mapcar #'.neko-id self))

(defmethod ig:push-id ((self neko))
  (ig:push-id (.neko-id self)))

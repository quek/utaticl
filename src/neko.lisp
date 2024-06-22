(in-package :dgw)

(defvar *neko-map* (make-hash-table :weakness :value :test 'equal))

(defun find-neko (neko-id)
  (gethash neko-id *neko-map*))

(defmethod initialize-instance :after ((self neko) &key)
  (unless (slot-boundp self 'neko-id)
    (loop for uid = (uid)
          unless (gethash uid *neko-map*)
            do (setf (slot-value self 'neko-id) uid)
               (loop-finish)))
  (setf (gethash (slot-value self 'neko-id) *neko-map*) self))

(defmethod copy ((self neko))
  (let ((serialized (with-serialize-context
                      (serialize self))))
    (setf (getf (cdr serialized) 'neko-id) (uid))
    (with-serialize-context
      (deserialize serialized))))

(assert (let ((self (make-instance 'neko)))
          (string/= (.neko-id self)
                    (.neko-id (copy self)))))

(defmethod (setf .neko-id) :around (value (self neko))
  (let ((neko-id-old (.neko-id self)))
    (prog1 (call-next-method)
      (remhash neko-id-old *neko-map*)
      (setf (gethash value *neko-map*) self))))

(defmethod .neko-id ((self list))
  (mapcar #'.neko-id self))

(defmethod print-object ((self neko) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a"
            (.name self)
            (.neko-id self))))

(defmethod ig:push-id ((self neko))
  (ig:push-id (.neko-id self)))

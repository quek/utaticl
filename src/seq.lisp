(in-package :dgw)

(defmethod deserialize-neko ((self seq) slots)
  (let ((neko-id (getf slots 'neko-id)))
    (or (find-neko neko-id)
        (call-next-method))))

;;; TODO これいらない？
(defmethod deserialize-slot ((self seq) (slot (eql 'neko-id)) value)
  ;; see deserialize-slot ((self neko) (slot (eql 'neko-id)) value)
  ;; (.copy *serialize-context*) でも新し neko-id にしない
  (setf (.neko-id self) (deserialize value)))

(in-package :dgw)

(defmethod connect ((from module) (to module))
  (let ((connection (make-instance 'connection
                                   :from from
                                   :to to)))
    (push connection (.connections from))
    (push connection (.connections to))))

(defmethod connections-from ((self module))
  (loop for connection in (.connections self)
        if (eq (.to connection) self)
          collect connection))

(defmethod connections-to ((self module))
  (loop for connection in (.connections self)
        if (eq (.from connection) self)
          collect connection))

(defmethod editor-open ((self module))
  (setf (.editor-open-p self) t))

(defmethod editor-close ((self module))
  (setf (.editor-open-p self) nil))

(defmethod prepare ((self module))
  (setf (.process-done self) nil))

(defmethod process-connection ((self module))
  ;; TODO
  )

(defmethod process ((self module))
  (setf (.process-done self) t))

(defmethod start ((self module))
  (setf (.start-p self) t))

(defmethod stop :before ((self module))
  (editor-close self))

(defmethod stop ((self module))
  (setf (.start-p self) nil))

(defmethod terminate :before ((self module))
  (stop self))

(defmethod terminate ((self module)))

(defmethod wait-for-from-p ((self module))
  (some (complement #'.process-done) (connections-from self)))

(defmethod wait-for-to-p ((self module))
  (some (complement #'.process-done) (connections-to self)))

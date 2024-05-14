(in-package :dgw)

(defmethod connect ((from module) (to module)
                    from-process-data to-process-data)
  (let ((connection (make-instance 'connection
                                   :from from
                                   :to to
                                   :from-bus-index 0
                                   :to-bus-index 0
                                   :from-process-data from-process-data
                                   :to-process-data to-process-data)))
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

(defmethod deserialize-after ((self module))
  (initialize self)
  (start self))

(defmethod deserialize-slot ((self module) (slot (eql 'state)) value)
  (setf (state self) value))

(defmethod editor-open ((self module))
  (setf (.editor-open-p self) t))

(defmethod editor-close ((self module))
  (setf (.editor-open-p self) nil))

(defmethod initialize ((self module)))

(defmethod prepare ((self module))
  (setf (.process-done self) nil))

(defmethod process-connection ((self module))
  (loop for connection-from in (connections-from self)
        do (process connection-from)))

(defmethod process ((self module))
  (setf (.process-done self) t))

(defmethod serialize ((self module))
  (append (call-next-method)
          `(state ,(state self))))

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
  (some (lambda (connection)
          (not (.process-done (.from connection))))
        (connections-from self)))

(defmethod wait-for-to-p ((self module))
  (some (lambda (connection)
          (not (.process-done (.to connection))))
        (connections-to self)))


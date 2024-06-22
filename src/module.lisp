(in-package :dgw)

(defmethod connect ((from module) (to module))
  (let ((connection (make-instance 'connection
                                        :from from
                                        :to to
                                        :from-bus-index 0
                                        :to-bus-index 0)))
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


(defmethod disconnect ((from module) (to module))
  "from is a fader, to is a gain"
  (flet ((pred (connection)
           (and (eq (.from connection) from)
                (eq (.to connection) to))))
    (setf (.connections from)
          (delete-if #'pred (.connections from)))
    (setf (.connections to)
          (delete-if #'pred (.connections to)))))

(defmethod editor-open ((self module))
  (setf (.editor-open-p self) t))

(defmethod editor-close ((self module))
  (setf (.editor-open-p self) nil))

(defmethod initialize ((self module)))

(defmethod (setf .latency-pdc) :after (value (self module))
  (loop for connection in (.connections self)
        if (eq self (.to connection))
          do (setf (.latency-pdc connection)
                   (if (< (.latency-pdc (.from connection)) value)
                       (- value (.latency-pdc (.from connection)))
                       0))))

(defmethod prepare ((self module))
  (setf (.process-done self) nil))

(defmethod process-connection ((self module))
  (loop for connection-from in (connections-from self)
        do (process connection-from)))

(defmethod process ((self module))
  (loop for connection in (connections-to self)
        do (setf (.from-process-data connection) *process-data*))
  (setf (.process-done self) t))

(defmethod .project ((self module))
  (.project (.track self)))

(defmethod serialize ((self module))
  (append (call-next-method)
          `(state ,(state self))))

(defmethod start ((self module))
  (setf (.start-p self) t))

(defmethod stop ((self module))
  (setf (.start-p self) nil))

(defmethod terminate :before ((self module))
  (editor-close self)
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


(in-package :dgw)

(defmethod editor-open ((self module))
  (setf (.editor-open-p self) t))

(defmethod editor-close ((self module))
  (setf (.editor-open-p self) nil))

(defmethod process-connection ((self module))
  ;; TODO
  )

(defmethod process ((self module))
  ;; TODO
  )

(defmethod start ((self module))
  (setf (.start-p self) t))

(defmethod stop :before ((self module))
  (editor-close self))

(defmethod stop ((self module))
  (setf (.start-p self) nil))

(defmethod terminate :before ((self module))
  (stop self))

(defmethod wait-for-from-p ((self module))
  ;; TODO
  nil)

(defmethod wait-for-to-p ((self module))
  ;; TODO
  nil)

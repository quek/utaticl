(in-package :dgw)

(defclass module ()
  ((start-p :initform nil :accessor .start-p)
   (editor-open-p :initform nil :accessor .editor-open-p)))

(defmethod editor-open ((self module))
  (setf (.editor-open-p self) t))

(defmethod editor-close ((self module))
  (setf (.editor-open-p self) nil))

(defmethod start ((self module))
  (setf (.start-p self) t))

(defmethod stop ((self module))
  (setf (.start-p self) nil))

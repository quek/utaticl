(in-package :utaticl.core)

(defclass ring-buffer ()
  ((buffer :initarg :size :accessor .buffer)
   (head :initform 0 :accessor .head)
   (tail :initform 0 :accessor .tail)))

(defmethod initialize-instance :after ((self ring-buffer) &key size)
  (setf (.buffer self)
        (make-array size :element-type 'single-float :initial-element .0)))

(defmethod ring-buffer-push ((self ring-buffer) value)
  (setf (aref (.buffer self) (.tail self)) value)
  (incf (.tail self))
  (when (= (length (.buffer self)) (.tail self))
    (setf (.tail self) 0)))

(defmethod ring-buffer-pop ((self ring-buffer))
  (prog1 (aref (.buffer self) (.head self))
    (incf (.head self))
    (when (= (length (.buffer self)) (.head self))
      (setf (.head self) 0))))

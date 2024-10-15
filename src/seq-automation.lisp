(in-package :utaticl.core)

(defmethod initialize-instance :after ((self seq-automation) &key)
  (when (string= "" (.name self))
    (setf (.name self) (name-new 'seq-automation "P"))))

(defmethod automation-point-add ((self seq-automation) (automation-point automation-point))
  (setf (.points self)
        (sort (cons automation-point
                    (copy-list (.points self)))
              (lambda (x y)
                (if (= (.time x) (.time y))
                    (< (.value x) (.value y))
                    (< (.time x) (.time y)))))))

(defmethod automation-point-delete ((self seq-automation) (automation-point automation-point))
  (setf (.points self)
        (remove automation-point (.points self))))

(in-package :dgw)

(defmethod clip-add ((self lane) clip)
  (setf (.clips self)
        (sort (cons clip (.clips self))
              (lambda (x y)
                (< (.time x) (.time y))))))

(defmethod clip-delete ((self lane) clip)
  (setf (.clips self)
        (delete clip (.clips self))))

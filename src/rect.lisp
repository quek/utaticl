(in-package :utaticl.core)

(defmethod initialize-instance :after ((self rect) &key min max)
  (when min
    (setf (.x1 self) (.x min))
    (setf (.y1 self) (.y min)))
  (when max
    (setf (.x2 self) (.x max))
    (setf (.y2 self) (.y max))))

(defmethod include-p ((self rect) point)
  (and (<= (.x1 self) (.x point))
       (<= (.y1 self) (.y point))
       (<= (.x point) (.x2 self))
       (<= (.y point) (.y2 self))))

(defmethod print-object ((self rect) stream)
  (format stream "(~a ~a ~a ~a)" (.x1 self) (.y1 self) (.x2 self) (.y2 self)))

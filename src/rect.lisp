(in-package :utaticl.core)

(defun @@ (min-or-x1 max-or-y1 &optional x2 y2)
    (if (consp min-or-x1)
        (make-instance 'rect :min min-or-x1 :max max-or-y1)
        (make-instance 'rect :min (@ min-or-x1 max-or-y1)
                             :max (@ x2 y2))))

(defmethod initialize-instance :after ((self rect) &key)
  (when (> (.x (.min self)) (.x (.max self)))
    (psetf (.x (.min self)) (.x (.max self))
           (.x (.max self)) (.x (.min self))))
  (when (> (.y (.min self)) (.y (.max self)))
    (psetf (.y (.min self)) (.y (.max self))
           (.y (.max self)) (.y (.min self)))))

(defmethod include-p ((self rect) point)
  (and (<= (.x1 self) (.x point))
       (<= (.y1 self) (.y point))
       (<= (.x point) (.x2 self))
       (<= (.y point) (.y2 self))))

(defmethod overlap ((self rect-piano-roll) (note note))
  (if (and (<= (.x1 self) (.key note) (.x2 self))
           (< (.y1 self) (time-end note))
           (< (.time note) (.y2 self)))
      (@@ (max (.y1 self) (.time note))
          (.key note)
          (min (.y2 self) (time-end note))
          (.key note))
      nil))

(defmethod print-object ((self rect) stream)
  (format stream "(~a ~a ~a ~a)" (.x1 self) (.y1 self) (.x2 self) (.y2 self)))

(defmethod .x1 ((self rect))
  (.x (.min self)))

(defmethod .y1 ((self rect))
  (.y (.min self)))

(defmethod .x2 ((self rect))
  (.x (.max self)))

(defmethod .y2 ((self rect))
  (.y (.max self)))

(defmethod .width ((self rect))
  (- (.x2 self) (.x1 self)))

(defmethod .height ((self rect))
  (- (.y2 self) (.y1 self)))

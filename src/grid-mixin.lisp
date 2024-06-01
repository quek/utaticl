(in-package :dgw)

(defmethod render-grid ((self grid-mixin))
  (ig:set-next-item-width 60.0)
  (ig:combo "##grid" (.grid-unit self) *grid-all* :item-display-function #'grid-name)
  (ig:same-line)
  (button-toggle "Snap" (.grid-snap-p self)))


(defmethod time-grid-applied ((self grid-mixin) time snap-function)
  (if (.grid-snap-p self)
      (let ((unit (.grid-unit self)))
        (* (funcall snap-function (/ time unit))
           unit))
      time))

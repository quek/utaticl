(in-package :dgw)

(defmethod render-grid ((self grid-mixin))
  (ig:set-next-item-width 60.0)
  (ig:combo "##grid" (.grid-unit self) *grid-all* :item-display-function #'grid-name)
  (ig:same-line)
  (button-toggle "Snap" (.grid-snap-p self)))


(defmethod time-grid-applied ((self grid-mixin) time snap-function)
  (let ((unit (.grid-unit self)))
    (if (and (.grid-snap-p self)
             (not (eq unit +grid-none+)))
        (* (funcall snap-function (/ time unit))
           unit)
        time)))

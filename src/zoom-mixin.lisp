(in-package :dgw)

(defmethod time-to-local-x ((self zoom-mixin) time)
  (coerce (* time (.zoom-x self)) 'single-float))

(defmethod time-to-world-x ((self zoom-mixin) time)
  (+ (time-to-local-x self time) (.x (ig:get-window-pos))))

(defmethod zoom-x-update ((self zoom-mixin) io)
  (when (and (/= .0 (c-ref io ig:im-gui-io :mouse-wheel))
             (ig:ensure-to-bool (c-ref io ig:im-gui-io :key-ctrl))
             (ig:ensure-to-bool (c-ref io ig:im-gui-io :key-alt)))
    (setf (.zoom-x self)
          (max (+ (.zoom-x self) (* (c-ref io ig:im-gui-io :mouse-wheel) .5))
               .1))))

(defmethod zoom-y-update ((self zoom-mixin) io)
  (when (and (/= .0 (c-ref io ig:im-gui-io :mouse-wheel))
             (ig:ensure-to-bool (c-ref io ig:im-gui-io :key-ctrl)))
    (setf (.zoom-y self)
          (max (+ (.zoom-y self) (* (c-ref io ig:im-gui-io :mouse-wheel) .5))
               .1))))

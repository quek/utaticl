(in-package :dgw)

(flet ((%time-to-local-y (self time)
         (coerce (* time (.zoom-y self)) 'single-float)))

  (defmethod time-to-local-y ((self zoom-mixin) time)
    (%time-to-local-y self time))

  (defmethod time-to-world-y ((self zoom-mixin) time)
    (+ (%time-to-local-y self time) (.y (ig:get-window-pos)))))

(defmethod zoom-x-update ((self zoom-mixin) io)
  (when (and (/= .0 (c-ref io ig:im-gui-io :mouse-wheel))
             (ig:ensure-to-bool (c-ref io ig:im-gui-io :key-ctrl))
             (ig:ensure-to-bool (c-ref io ig:im-gui-io :key-alt)))
    (setf (.zoom-x self)
          (max (+ (.zoom-x self) (* (c-ref io ig:im-gui-io :mouse-wheel)
                                    (.zoom-x-factor self)))
               (.zoom-x-min self)))))

(defmethod zoom-y-update ((self zoom-mixin) io)
  (when (and (/= .0 (c-ref io ig:im-gui-io :mouse-wheel))
             (ig:ensure-to-bool (c-ref io ig:im-gui-io :key-ctrl))
             (not (ig:ensure-to-bool (c-ref io ig:im-gui-io :key-alt))))
    (setf (.zoom-y self)
          (max (+ (.zoom-y self) (* (c-ref io ig:im-gui-io :mouse-wheel)
                                    (.zoom-y-factor self)))
               (.zoom-y-min self)))))

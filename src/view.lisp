(in-package :utaticl.core)

(defmethod can-handle-mouse-p ((self view))
  (ig:is-window-hovered (logior ig:+im-gui-hovered-flags-child-windows+
                                ig:+im-gui-hovered-flags-allow-when-blocked-by-active-item+)))

(defmethod local-to-world ((self view) pos)
  (@+ pos *window-pos* (@ (- *scroll-x*) (- *scroll-y*))))

(defmethod rect-body ((self view))
  (@@ (@+ *window-pos* (@ (.offset-x self) (.offset-y self)))
      (@+ *window-pos* *window-size* (@ 0.0 (- *scrollbar-size*)))))

(defmethod world-y-to-time ((self view) y &optional grid-snap)
  (let ((time
          (/ (coerce (+ (- y (.y (ig:get-window-pos)) (.offset-y self))
                        (ig:get-scroll-y))
                     'double-float)
             (coerce (.zoom-y self) 'double-float))))
    (if grid-snap
        (time-grid-applied self time grid-snap)
        time)))





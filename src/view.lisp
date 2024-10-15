(in-package :utaticl.core)

(defmethod can-handle-mouse-p ((self view))
  (ig:is-window-hovered (logior ig:+im-gui-hovered-flags-child-windows+
                                ig:+im-gui-hovered-flags-allow-when-blocked-by-active-item+)))

(defmethod rect-body ((self view))
  (@@ (@+ *window-pos* (@ (.offset-x self) (.offset-y self)))
      (@+ *window-pos* *window-size* (- 0.0 (.y *scrollbar-size*)))))

(in-package :dgw)

(defmethod can-handle-mouse-p ((self view))
  (ig:is-window-hovered (logior ig:+im-gui-hovered-flags-child-windows+
                                ig:+im-gui-hovered-flags-allow-when-blocked-by-active-item+)))

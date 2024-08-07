(in-package :dgw)

(defmethod can-handle-mouse-p ((self view))
  (ig:is-window-hovered ig:+im-gui-hovered-flags-child-windows+))

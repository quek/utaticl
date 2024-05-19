(in-package :dgw)

(defmethod can-handle-mouse-p ((self view))
  (ig:is-window-hovered ig:+im-gui-hovered-flags-child-windows+))

(defmethod handle-mouse :around ((self view))
  (when (can-handle-mouse-p self)
    (call-next-method)))

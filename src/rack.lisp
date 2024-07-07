(in-package :dgw)

(defmethod initialize-instance :after ((self rack) &key)
  (let ((plugin-selector (make-instance 'plugin-selector :rack self)))
    (setf (.plugin-selector self) plugin-selector)))

(defmethod render ((self rack))
  (ig:with-begin ("##rack" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (ig:with-begin-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

      (loop for module in (.modules (.target-track (.project self)))
            do (ig:with-group (render module))
               (ig:same-line))

      (when (ig:button "+")
        (open-plugin-selector (.plugin-selector self)))
      (render (.plugin-selector self))

      (shortcut-common (.project self)))))

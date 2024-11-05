(in-package :utaticl.core)

(defmethod initialize-instance :after ((self rack) &key)
  (let ((plugin-selector (make-instance 'plugin-selector :rack self)))
    (setf (.plugin-selector self) plugin-selector)))

(defmethod render ((self rack))
  (ig:with-begin ("##rack" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (ig:with-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
      (with-window-info (self)
        (loop for module in (.modules (.target-track (.project self)))
              with first-p = t
              if first-p
                do (setf first-p nil)
              else
                do (ig:same-line)
              end
              if (typep module 'module-fader-track)
                do (when (ig:button "+")
                     (open-plugin-selector (.plugin-selector self)))
                   (ig:same-line)
              do (ig:with-group (render module))
                 (ig:same-line))

        (render (.plugin-selector self))

        (shortcut-common (.project self))))))

(defmethod .offset-x ((self rack))
  0.0)

(defmethod .offset-y ((self rack))
  0.0)

(defmethod .zoom-x ((self rack))
  1.0)

(defmethod .zoom-y ((self rack))
  1.0)

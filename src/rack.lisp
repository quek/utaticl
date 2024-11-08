(in-package :utaticl.core)

(defmethod initialize-instance :after ((self rack) &key)
  (let ((plugin-selector (make-instance 'plugin-selector :rack self)))
    (setf (.plugin-selector self) plugin-selector)))

(defmethod render ((self rack))
  (ig:with-begin ("##rack" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (ig:with-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
      (with-window-info (self)
        (let ((fader (find-if (lambda (x) (typep x 'module-fader-track))
                              (.modules (.target-track *project*)))))
          (render-in fader self))
        (ig:same-line)
        (loop for module in (.modules (.target-track *project*))
              unless (typep module 'module-fader-track)
              do (ig:with-group (render-in module self))
                 (ig:same-line))
        (when (ig:button "+" (@ 0.0 (- (.y *window-size*)
                                       *item-spacing-y*
                                       *scrollbar-size*)))
          (open-plugin-selector (.plugin-selector self)))

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

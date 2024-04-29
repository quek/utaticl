(in-package :dgw)

(defmethod render ((self rack))
  (when (ig:begin "##rack" (cffi:null-pointer) ig:+im-gui-window-flags-no-scrollbar+)
    (when (ig:begin-child "##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

      (loop for module in (.modules (.target-track *project*))
            do (when (ig:button (.name module))
                 (if (.editor-open-p module)
                     (editor-close module)
                     (editor-open module)))
               (ig:same-line))

      (when (ig:button "+")
        (open-plugin-selector (.plugin-selector self)))
      (render (.plugin-selector self))

      (ig:end-child)
      (shortcut-common))
    (ig:end)))

(in-package :dgw)

(defmethod render ((self rack))
  (when (ig:begin "##rack" (cffi:null-pointer) ig:+im-gui-window-flags-no-scrollbar+)
    (when (ig:begin-child "##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
      
      (loop for module in (.modules (.target-track *project*))
            do (ig:text (.name module))
               (ig:same-line))

      (when (ig:button "+")
        (show-plugin-selector self)))
    (ig:end-child)
    (shortcut-common))
  (ig:end)

  (render (.plugin-selector self)))

(defmethod show-plugin-selector ((self rack))
  (show (.plugin-selector self)))

(in-package :dgw)

(defun open-plugin-selector (self)
  (let ((plugin-infos (plugin-info-load-all)))
    (setf (.plugin-infos self)
          (sort plugin-infos (lambda (x y)
                               (string< (string-upcase (.name x))
                                        (string-upcase (.name y)))))))

  (ig:open-popup "Plugin Selector"))

(defmethod .project ((self plugin-selector))
  (.project (.rack self)))

(defmethod render ((self plugin-selector))
  (ig:set-next-window-size-constraints (@ 300.0 200.0) (@ ig:+flt-max+ ig:+flt-max+))

  (when (ig:begin-popup-modal "Plugin Selector")
    (when (ig:is-window-appearing)
      (ig:set-keyboard-focus-here))

    (let ((run-p (ig:input-text "##query" (.query self)
                                :flags (logior ig:+im-gui-input-text-flags-auto-select-all+
                                               ig:+im-gui-input-text-flags-enter-returns-true+))))
      (loop for plugin-info in (.plugin-infos self)
            if (fuzzy= (.name plugin-info) (.query self))
              do (if run-p
                     #1=(progn
                          (cmd-add (.project self) 'cmd-module-add
                                   :track-id (.neko-id (.target-track (.project self)))
                                   :plugin-info plugin-info
                                   :before (fader (.target-track (.project self))))
                          (ig:close-current-popup)
                          (loop-finish))
                     (progn
                       (when (ig:button (.name plugin-info))
                         #1#)))))

    (when (ig:is-key-pressed ig:+im-gui-key-escape+)
      (ig:close-current-popup))

    (ig:end-popup)))

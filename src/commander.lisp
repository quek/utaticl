(in-package :dgw)

(defmethod render ((self commander))
  (when (.show-p self)
    (ig:open-popup-str "Commander" 0))
  (when (ig:begin-popup-modal "Commander" :open-p (.show-p self))
    (when (ig:is-window-appearing)
      (ig:set-keyboard-focus-here))
    (let ((run (ig:input-text "##query" (.query self)
                      :flags (logior  ig:+im-gui-input-text-flags-auto-select-all+
                                      ig:+im-gui-input-text-flags-enter-returns-true+))))
      (let ((classes (sb-mop:class-direct-subclasses (find-class 'command))))
        (loop for class in classes
              for class-name = (symbol-name (class-name class))
              do (ig:text class-name))))
    (ig:end-popup)))

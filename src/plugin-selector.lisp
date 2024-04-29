(in-package :dgw)

(defmethod show :before ((self plugin-selector))
  (let ((path (merge-pathnames "user/config/plugins.lisp" *working-directory*)))
    (when (probe-file path)
      (let ((plugin-infos (with-open-file (in path)
                            (loop for sexp = (read in nil nil)
                                  while sexp
                                  collect (deserialize sexp)))))
        (setf (.plugin-infos self)
              (sort plugin-infos (lambda (x y)
                                   (string< (.name x) (.name y)))))))))

(defmethod render ((self plugin-selector))
  (when (.show-p self)
    (ig:open-popup "Plugin Selector")

    (ig:set-next-window-size-constraints (@ 300.0 200.0) (@ ig:+flt-max+ ig:+flt-max+))
    
    (when (ig:begin-popup-modal "Plugin Selector" :open-p (.show-p self))
      (when (ig:is-window-appearing)
        (ig:set-keyboard-focus-here))
      
      (let ((run-p (ig:input-text "##query" (.query self)
                                  :flags (logior ig:+im-gui-input-text-flags-auto-select-all+
                                                 ig:+im-gui-input-text-flags-enter-returns-true+))))
        (loop for plugin-info in (.plugin-infos self)
              if (fuzzy= (.name plugin-info) (.query self))
                do (if run-p
                       #1=(progn
                            (cmd-add *project* 'cmd-plugin-add
                                     :track-id (.neko-id (.target-track *project*))
                                     :plugin-info plugin-info)
                            (hide self)
                            (loop-finish))
                       (progn
                         (when (ig:button (.name plugin-info))
                           #1#)))))
      
      (when (ig:is-key-pressed ig:+im-gui-key-escape+)
        (hide self))
      
      (ig:end-popup))))
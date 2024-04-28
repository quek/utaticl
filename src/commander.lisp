(in-package :dgw)

(defmethod render ((self commander))
  (when (.show-p self)
    (ig:open-popup-str "Commander" 0))
  
  (ig:set-next-window-size-constraints (@ 300.0 200.0) (@ ig:+flt-max+ ig:+flt-max+))
  
  (when (ig:begin-popup-modal "Commander" :open-p (.show-p self))
    (when (ig:is-window-appearing)
      (ig:set-keyboard-focus-here))
    
    (let ((run (ig:input-text "##query" (.query self)
                              :flags (logior  ig:+im-gui-input-text-flags-auto-select-all+
                                              ig:+im-gui-input-text-flags-enter-returns-true+))))
      (let ((classes (sb-mop:class-direct-subclasses (find-class 'command))))
        (loop for class in classes
              for class-name = (subseq (substitute #\space #\- (symbol-name (class-name class)))
                                       4) ;"CMD-" を削除
              if (fuzzy= class-name (.query self))
                do (if run
                       (progn
                         (cmd-add *project* class)
                         (setf (.show-p self) nil))
                       (when (ig:button class-name)
                         (cmd-add *project* class)
                         (setf (.show-p self) nil))))))
    
    (when (ig:is-key-pressed ig:+im-gui-key-escape+)
      (setf (.show-p self) nil))
    
    (ig:end-popup)))

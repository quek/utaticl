(in-package :dgw)

(defmethod render ((self commander))
  (when (.show-p self)
    (ig:open-popup "Commander"))

  (ig:set-next-window-size-constraints (@ 300.0 200.0) (@ ig:+flt-max+ ig:+flt-max+))

  (ig:with-popup-modal ("Commander" :open-p (.show-p self))
    (when (ig:is-window-appearing)
      (ig:set-keyboard-focus-here))

    (let ((run (ig:input-text "##query" (.query self)
                              :flags (logior  ig:+im-gui-input-text-flags-auto-select-all+
                                              ig:+im-gui-input-text-flags-enter-returns-true+))))
      (let* ((classes (loop for class in (sb-mop:class-direct-subclasses (find-class 'command))
                            for name = (subseq (substitute #\space #\- (symbol-name (class-name class)))
                                               4) ;"CMD-" を削除
                            if (fuzzy= name (.query self))
                              collect (cons class name)))
             (classes (sort classes #'string<
                            :key (lambda (x)
                                   (symbol-name (class-name (car x)))))))
        (loop for (class . name) in classes
              do (if run
                     (progn
                       (cmd-add (.project self) class)
                       (hide self)
                       (loop-finish))
                     (when (ig:button name)
                       (cmd-add (.project self) class)
                       (hide self))))))

    (when (ig:is-key-pressed ig:+im-gui-key-escape+)
      (hide self))))

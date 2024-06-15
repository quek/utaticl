(in-package :dgw)

(defmethod render ((self rack))
  (ig:with-begin ("##rack" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (ig:with-begin-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

      (loop for module in (.modules (.target-track *project*))
            do (ig:begin-group)
               (ig:with-id (module)
                 (when (ig:button (.name module))
                   (if (.editor-open-p module)
                       (editor-close module)
                       (editor-open module)))

                 ;; TODO state テスト用なので後で消す
                 (ig:with-popup-context-item ()
                   (when (ig:menu-item "Copy" :shortcut "C-c")
                     (let ((state (state module)))
                       (ig:set-clipboard-text state)))
                   (when (ig:menu-item "Paste" :shortcut "C-v")
                     (let ((state (ig:get-clipboard-text)))
                       (setf (state module) state))))

                 (render-module-delete-button self module))
               (ig:end-group)
               (ig:same-line))

      (when (ig:button "+")
        (open-plugin-selector (.plugin-selector self)))
      (render (.plugin-selector self))

      (shortcut-common))))

(defmethod render-module-delete-button ((self rack) (module module-track-mixin)))

(defmethod render-module-delete-button ((self rack) module)
  (when (ig:button "x")
    (cmd-add *project* 'cmd-module-delete
             :track-id (.neko-id (.target-track *project*))
             :module-id (.neko-id module))))

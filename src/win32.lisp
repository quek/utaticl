(in-package :win32)

(cffi:defcfun (adjust-window-rect-ex "AdjustWindowRectEx") :boolean
  (lp-rect :pointer)
  (dw-style :uint32)
  (b-menu :boolean)
  (dw-ex-style :uint32))

(ftw:defwndproc wnd-proc (hwnd msg wparam lparam)
  (let ((module (gethash (cffi:pointer-address hwnd) dgw::*hwnd-module-vst3-map*)))
    (when module
      (ftw:switch msg
        (ftw::+wm-size+
         (when (/= wparam ftw::+size-minimized+)
           (let* ((rect (ftw:get-client-rect hwnd))
                  (width (- (ftw:rect-right rect) (ftw:rect-left rect)))
                  (height (- (ftw:rect-bottom rect) (ftw:rect-top rect))))
             (dgw::on-resize module width height))))
        (ftw::+wm-destroy+
         (dgw::editor-close module)))))
  (ftw:default-window-proc hwnd msg wparam lparam))

(sb-ext:defglobal *registered-class* nil)

(defun make-window (width height resizable)
  (unless *registered-class*
    (setf *registered-class*
          (ftw:register-class "VST3 Editor" (cffi:callback wnd-proc)
                              :styles ftw::+cs-dblclks+)))

  (let ((ex-styles ftw::+ws-ex-appwindow+)
        (styles (logior ftw::+ws-overlapped+
                        ftw::+ws-caption+
                        ftw::+ws-sysmenu+
                        ftw::+ws-clipsiblings+)))
    (when resizable
      (setf styles (logior styles
                           ftw::+ws-sizebox+
                           ftw::+ws-maximizebox+
                           ftw::+ws-minimizebox+)))

    (cffi:with-foreign-objects ((rect '(:struct ftw::rect)))
      (ftw:rect-foreign (ftw:make-rect :left 0 :top 0 :right width :bottom height)
                        rect)
      (adjust-window-rect-ex rect styles 0 ex-styles)

      (let* ((rect (ftw:foreign-rect rect (ftw:make-rect)))
             (height (- (ftw:rect-bottom rect) (ftw:rect-top rect)))
             (width (- (ftw:rect-right rect) (ftw:rect-left rect)))
             (hwnd (ftw:create-window "VST3 Editor"
                                      :window-name "VST3 Editor"
                                      :ex-styles ex-styles
                                      :styles styles
                                      :height height
                                      :width width
                                      ;; エディタウインドが前面にとどまるように
                                      :parent dgw::*hwnd*)))
        (ftw:set-window-pos hwnd :top 0 0 0 0
                            '(:no-size :no-move :no-copy-bits :show-window))
        hwnd))))

(defun resize (hwnd width height)
  (let ((client-rect (ftw:get-client-rect hwnd)))
    (unless (and (= width (- (ftw:rect-right client-rect)
                             (ftw:rect-left client-rect)))
                 (= height (- (ftw:rect-bottom client-rect)
                              (ftw:rect-top client-rect))))
      (let ((window-info (ftw:get-window-info hwnd)))
        (cffi:with-foreign-objects ((rect '(:struct ftw::rect)))
          (ftw:rect-foreign (ftw:make-rect :left 0 :top 0 :right width :bottom height)
                            rect)
          (adjust-window-rect-ex rect
                                 (ftw:info-style window-info)
                                 0
                                 (ftw:info-ex-style window-info))
          (let ((rect (ftw:foreign-rect rect (ftw:make-rect))))
            (ftw:set-window-pos hwnd :top 0 0
                                (- (ftw:rect-right rect) (ftw:rect-left rect))
                                (- (ftw:rect-bottom rect) (ftw:rect-top rect))
                                '(:no-move :no-copy-bits :no-activate))))))))
#+nil
(make-window 1024 760 t)

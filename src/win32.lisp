(in-package :win32)

(cffi:defcfun (adjust-window-rect-ex "AdjustWindowRectEx") :boolean
  (lp-rect :pointer)
  (dw-style :uint32)
  (b-menu :boolean)
  (dw-ex-style :uint32))


(ftw:defwndproc wnd-proc (hwnd msg wparam lparam)
  (ftw:default-window-proc hwnd msg wparam lparam))

(defvar *registered-class* nil)

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
                                      :width width)))
        (ftw:set-window-pos hwnd :top 0 0 0 0
                            '(:no-size :no-move :no-copy-bits :show-window))
        hwnd))))

#+nil
(make-window 1024 760 t)

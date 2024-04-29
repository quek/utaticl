(in-package :dgw)

(defun main ()
  (sb-thread:make-thread
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
       (setf *app* (make-instance 'app))
       (with-audio
         (start-audio)
         (win32-main *app*)
         (terminate *app*))))
   :name "DGW"))

(cffi:defcfun ("ImGui_ImplWin32_WndProcHandler" impl-win32-wnd-proc-handler) :pointer
  (hwnd :pointer)
  (msg :unsigned-int)
  (wparam :pointer)
  (lparam :pointer))

(ftw:defwndproc dgw-wnd-proc (hwnd msg wparam lparam)
  (let ((ret (impl-win32-wnd-proc-handler hwnd msg wparam lparam)))
    (if (not (cffi:null-pointer-p ret))
        ret
        (progn
          (ftw:switch msg
            (ftw::+wm-destroy+
             (setf *done* nil)))
          (ftw:default-window-proc hwnd msg wparam lparam)))))

(defun win32-main (app)
  (ftw:register-class "DGW" (cffi:callback dgw-wnd-proc))
  (setf *hwnd* (ftw:create-window "DGW"))
  )

(defun create-device-d3d (hwnd)
  )

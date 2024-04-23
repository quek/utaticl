(in-package :dgw)

(sb-ext:defglobal *done* nil)
;;(setf *done* t)


#+nil
(progn
  (sdl2:init sdl2-ffi::+sdl-init-video+)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-flags+ 0)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-profile-mask+ sdl2-ffi::+sdl-gl-context-profile-core+)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-major-version+ 3)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-minor-version+ 2)
  (sdl2:set-hint :render-driver "opengl")
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-depth-size+ 24)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-stencil-size+ 8)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-doublebuffer+ 1)
  (sdl2:get-current-display-mode 0))
#+nil
(let* ((window (sdl2:create-window :title "DGW" :w 1024 :h 768
                                   :flags (list sdl2-ffi:+sdl-window-shown+
                                                sdl2-ffi:+sdl-window-opengl+
                                                sdl2-ffi:+sdl-window-resizable+)))
       (gl-context (sdl2:gl-create-context window)))
  (sdl2:gl-set-swap-interval 1)       ;enable vsync
  (let* ((ctx (ig::create-context (cffi:null-pointer))))
    (ig::set-current-context ctx)
    (ig-backend::impl-sdl2-init-for-opengl
     (autowrap:ptr window)
     (autowrap:ptr gl-context))
    ))


(defun gui-loop (app window gl-context e)
  (loop while (/= (sdl2-ffi.functions:sdl-poll-event e) 0)
        do (ig-backend::impl-sdl2-process-event (autowrap:ptr e))
           (if (eq (sdl2:get-event-type e) sdl2-ffi::+sdl-quit+)
               (setf *done* t)
               (if (and (eq (sdl2:get-event-type e) sdl2-ffi::+sdl-windowevent+)
                        (sdl2::c-let ((event sdl2-ffi:sdl-event :from e))
                          (sdl2::c-let ((we sdl2-ffi:sdl-window-event :from (event :window)))
                            (and (= (we :event) sdl2-ffi::+sdl-windowevent-close+)
                                 (= (we :window-id) (sdl2:get-window-id window))))))
                   (setf *done* t))))
  (ig-backend::impl-opengl3-new-frame)
  (ig-backend::impl-sdl2-new-frame)
  (ig::new-frame)

  (render app)
  
  (ig::render)
  (sdl2:gl-make-current window gl-context)
  (opengl:viewport 0 0
                   ;; TODO io->DislplaySize.x, io->DisplaySize.y
                   1024 768)
  (opengl:clear-color 0.45 0.55 0.60 1.0) ;TODO clearColor
  (opengl:clear :color-buffer-bit)
  (ig-backend::impl-opengl3-render-draw-data (autowrap:ptr (ig::get-draw-data)))
  (sdl2:gl-swap-window window))

(defun sdl2-main (app)
  "https://github.com/cimgui/cimgui/blob/docking_inter/backend_test/example_sdl_opengl3/main.c"
  (sdl2:init (logior sdl2-ffi:+sdl-init-video+))
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-flags+ 0)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-profile-mask+ sdl2-ffi::+sdl-gl-context-profile-core+)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-major-version+ 3)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-context-minor-version+ 0)

  ;; (sdl2:set-hint sdl2-ffi:+sdl-hint-ime-show-ui+ "1")
  
  (sdl2:set-hint :render-driver "opengl")
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-depth-size+ 24)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-stencil-size+ 8)
  (sdl2:gl-set-attr sdl2-ffi::+sdl-gl-doublebuffer+ 1)
  (sdl2:get-current-display-mode 0)
  (let* ((window (sdl2:create-window :title "DGW" :w 1024 :h 768
                                     :flags (list sdl2-ffi:+sdl-window-shown+
                                                  sdl2-ffi:+sdl-window-opengl+
                                                  sdl2-ffi:+sdl-window-resizable+)))
         ;; FIXMI なぜか初回の sdl2:create-window だと窓が表示されない・・・
         (window (progn
                   (sdl2:destroy-window window)
                   (sdl2:create-window :title "DGW" :w 1024 :h 768
                                       :flags (list sdl2-ffi:+sdl-window-shown+
                                                    sdl2-ffi:+sdl-window-opengl+
                                                    sdl2-ffi:+sdl-window-resizable+))))
         (gl-context (sdl2:gl-create-context window)))
    (sdl2:gl-set-swap-interval 1)       ;enable vsync
    (let* ((ctx (ig::create-context (cffi:null-pointer))))
      ;; TODO ImGuiIO の設定
      
      (ig-backend::impl-sdl2-init-for-opengl
       (autowrap:ptr window)
       (autowrap:ptr gl-context))
      (ig-backend::impl-opengl3-init "#version 130")

      (setf *done* nil)
      (sdl2:with-sdl-event (e)
        (loop until *done* do
          (handler-case
              (gui-loop app window gl-context e)
            ;; TODO
            (CL-OPENGL-BINDINGS:OPENGL-ERROR ()))))
      
      (ig-backend::impl-opengl3-shutdown)
      (ig-backend::impl-sdl2-shutdown)
      (ig::destroy-context ctx)
      (sdl2:gl-delete-context gl-context)
      (sdl2:destroy-window window)
      (sdl2:quit))))

#+nil
(defun glfw-scratch-main ()
  (glfw:initialize)

  (let* ((window (glfw:create-window :width 1280 :height 720 :title "TITLE")))
    (glfw::make-context-current window)
    (glfw::swap-interval 1)             ;enable vsync
    (let ((ctx (ig::create-context (cffi:null-pointer))))
      ()
      (loop with *done* = nil
            until *done* do
              (ig::new-frame)
              (cffi:with-foreign-object (openp :bool)
                (setf (cffi:mem-ref openp :bool) t)
                (when (ig::begin "Hello" openp 0)
                  (ig::text "World!")
                  (cffi:with-foreign-object (size '(:struct ig::vec2))
                    (setf (cffi:foreign-slot-value size '(:struct ig::vec2) 'ig::x) 40.0
                          (cffi:foreign-slot-value size '(:struct ig::vec2) 'ig::y) 200.0)
                    (when (ig::button "Exit" size)
                      (setf *done* t)))))
              (ig::end)
              (ig::render)
              (ig::get-draw-data))
      (ig::destroy-context ctx))
    (glfw::destroy-window window))
  (glfw::terminate))

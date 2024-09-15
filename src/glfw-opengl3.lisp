(in-package :dgw.glfw-opengl3)

(defun main ()
  (glfw:set-error-callback 'glfw-error-callback)

  (pa:with-audio
    (autowrap:with-alloc(glyph-ranges 'ig:im-wchar 3)
      (glfw:with-init-window (:title "DGW" :width 1600 :height 1200
                              :context-version-major 3
                              :context-version-minor 0)
        (glfw:swap-interval 1) ;Enable vsync
        (let ((ig-context (ig-init glyph-ranges)))

          (ImGui_ImplGlfw_InitForOpenGL glfw:*window* t)
          (ImGui_ImplOpenGL3_Init "#version 130")

          (setf dgw::*app* (make-instance 'dgw::app :window glfw:*window*))
          (setf dgw::*hwnd* (glfwGetWin32Window glfw:*window*))

          (loop until (glfw:window-should-close-p)
                do (main-loop))

          (ImGui_ImplOpenGL3_Shutdown)
          (ImGui_ImplGlfw_Shutdown)
          (ig:destroy-context ig-context))))))

(defun main-loop ()
  (glfw:poll-events)
  (if (glfw:get-window-attribute :iconified)
      (sleep .01)
      (progn
        (ImGui_ImplOpenGL3_NewFrame)
        (ImGui_ImplGlfw_NewFrame)
        (ig:new-frame)
        (dgw::with-debugger
          (ig:show-demo-window (cffi:null-pointer))
          (let ((dgw::*render-context* (make-instance 'dgw::render-context)))
            (dgw::render dgw::*app*)
            (dgw::cmd-run dgw::*app*)))
        (ig::render)
        (handler-case
            (progn
              (destructuring-bind (width height) (glfw:get-framebuffer-size)
                (gl:viewport 0 0 width height))
              (gl:clear-color 0.45 0.55 0.60 0.80)
              (gl:clear :color-buffer-bit))
          (error (e) (print e)))
        (ImGui_ImplOpenGL3_RenderDrawData (autowrap:ptr (ig:get-draw-data)))
        (when (plusp (logand (plus-c:c-ref (ig:get-io) ig:im-gui-io :config-flags)
                             ig:+im-gui-config-flags-viewports-enable+))
          (let ((backup-current-context (glfw:get-current-context)))
            (ig:update-platform-windows)
            (ig:render-platform-windows-default (cffi:null-pointer) (cffi:null-pointer))
            (glfw:make-context-current backup-current-context)))
        (glfw:swap-buffers))))

(cffi:defcallback glfw-error-callback :void
    ((error :int)
     (description :string))
  (log:error "GLFW ~d ~a" error description))

(defun ig-init (glyph-ranges)
  (prog1 (ig:create-context (cffi:null-pointer))
    (let ((io (ig:get-io)))
      (ensure-directories-exist (merge-pathnames "user/config/" dgw::*working-directory*))
      (setf (plus-c:c-ref io ig:im-gui-io :ini-filename)
            (namestring (merge-pathnames "user/config/imgui.ini" dgw::*working-directory*)))
      (setf (plus-c:c-ref io ig:im-gui-io :config-docking-with-shift) 1)
      (setf (plus-c:c-ref io ig:im-gui-io :config-windows-move-from-title-bar-only) 1)
      (setf (plus-c:c-ref io ig:im-gui-io :config-flags)
            (logior (plus-c:c-ref io ig:im-gui-io :config-flags)
                    ig:+im-gui-config-flags-nav-enable-keyboard+
                    ig:+im-gui-config-flags-docking-enable+))
      (let ((font (namestring (merge-pathnames "factory/font/NotoSansJP-Regular.ttf"
                                               dgw::*working-directory*))))
        (setf (plus-c:c-ref glyph-ranges ig:im-wchar 0) #x0020
              (plus-c:c-ref glyph-ranges ig:im-wchar 1) #xfffd
              (plus-c:c-ref glyph-ranges ig:im-wchar 2) 0)
        (ig:im-font-atlas-add-font-from-file-ttf
         (plus-c:c-ref io ig:im-gui-io :fonts) font 16.0 (cffi:null-pointer) glyph-ranges)
        (log:info "Load ~A" font)))

    (ig:style-colors-dark (cffi:null-pointer))))

;;; bool ImGui_ImplGlfw_InitForOpenGL(GLFWwindow* window, bool install_callbacks)
(cffi:defcfun ("ImGui_ImplGlfw_InitForOpenGL" ImGui_ImplGlfw_InitForOpenGL) :bool
  (window :pointer)
  (install_callbacks :bool))

;;; bool    ImGui_ImplOpenGL3_Init(const char* glsl_version)
(cffi:defcfun ("ImGui_ImplOpenGL3_Init" ImGui_ImplOpenGL3_Init) :bool
  (glsl_version :string))

;;; void ImGui_ImplOpenGL3_NewFrame(void);
(cffi:defcfun ("ImGui_ImplOpenGL3_NewFrame" ImGui_ImplOpenGL3_NewFrame) :void)

;;; void ImGui_ImplGlfw_NewFrame(void);
(cffi:defcfun ("ImGui_ImplGlfw_NewFrame" ImGui_ImplGlfw_NewFrame) :void)

;;; void ImGui_ImplOpenGL3_RenderDrawData(ImDrawData* draw_data);
(cffi:defcfun ("ImGui_ImplOpenGL3_RenderDrawData" ImGui_ImplOpenGL3_RenderDrawData) :void
  (draw-data :pointer))

;;; void ImGui_ImplOpenGL3_Shutdown(void);
(cffi:defcfun ("ImGui_ImplOpenGL3_Shutdown" ImGui_ImplOpenGL3_Shutdown) :void)

;;; void ImGui_ImplGlfw_Shutdown(void);
(cffi:defcfun ("ImGui_ImplGlfw_Shutdown" ImGui_ImplGlfw_Shutdown) :void)

;;; HWND glfwGetWin32Window (GLFWwindow *window)
(cffi:defcfun ("glfwGetWin32Window" glfwGetWin32Window) :pointer
  (window :pointer))

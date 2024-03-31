(in-package :ig-backend)

(cffi:defcfun ("ImGui_ImplSDL2_InitForOpenGL" impl-sdl2-init-for-opengl) :void
  (window :pointer)
  (gl-context :pointer))

(cffi:defcfun ("ImGui_ImplOpenGL3_Init" impl-opengl3-init) :void
  (glsl-version :string))

(cffi:defcfun ("ImGui_ImplSDL2_ProcessEvent" impl-sdl2-process-event) :void
  (event :pointer))

(cffi:defcfun ("ImGui_ImplOpenGL3_NewFrame" impl-opengl3-new-frame) :void)

(cffi:defcfun ("ImGui_ImplSDL2_NewFrame" impl-sdl2-new-frame) :void)

(cffi:defcfun ("ImGui_ImplOpenGL3_RenderDrawData" impl-opengl3-render-draw-data) :void
  (draw-data :pointer))

(cffi:defcfun ("ImGui_ImplOpenGL3_Shutdown" impl-opengl3-shutdown) :void)

(cffi:defcfun ("ImGui_ImplSDL2_Shutdown" impl-sdl2-shutdown) :void)

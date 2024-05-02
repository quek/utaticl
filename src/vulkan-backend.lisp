(defpackage vulkan-backend
  (:use :cl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "vk"))

(in-package :vulkan-backend)

;; TODO cimgui-autowrap/src/lib.lisp で読み込んでいる
;; (cffi:load-foreign-library "c:/Users/ancient/quicklisp/local-projects/cimgui-autowrap/lib/cimgui/backend_test/example_sdl_vulkan/build/cimgui_sdl.dll")

(defconstant +uint64-max+ (1- (expt 2 64)))

(cffi:defcstruct imgui-impl-vulkan-init-info
  (instance vulkan:instance)
  (physical-device vulkan:physical-device)
  (device vulkan:device)
  (queue-family :uint32)
  (queue vulkan:queue)
  (descriptor-pool vulkan:descriptor-pool)
  (render-pass vulkan:render-pass)
  (min-image-count :uint32)
  (image-count :uint32)
  (msaa-samples vulkan:sample-count-flag-bits)
  (pipeline-cache vulkan:pipeline-cache)
  (subpass :uint32)
  (use-dynamic-rendering :bool)
  (pipeline-rendering-create-info (:struct vulkan:pipeline-rendering-create-info-khr))
  (allocator :pointer)
  (check-vk-result-fn :pointer)
  (min-allocation-size vulkan:device-size))

(cffi:defcstruct imgui-impl-vulkan-h-frame
  (command-pool vulkan:command-pool)
  (command-buffer vulkan:command-buffer)
  (fence vulkan:fence)
  (backbuffer vulkan:image)
  (backbuffer-view vulkan:image-view)
  (framebuffer vulkan:framebuffer))

(cffi:defcstruct imgui-impl-vulkan-h-frame-semaphores
  (image-acquired-semaphore vulkan:semaphore)
  (render-complete-semaphore vulkan:semaphore))

(cffi:defcstruct imgui-impl-vulkan-h-window
  (width :int)
  (height :int)
  (swapchain vulkan:swapchain-khr)
  (surface vulkan:surface-khr)
  (surface-format (:struct vulkan:surface-format-khr))
  (present-mode vulkan:present-mode-khr)
  (render-pass vulkan:render-pass)
  (pipeline vulkan:pipeline)
  (use-dynamic-rendering :bool)
  (clear-enable :bool)
  (clear-value (:union vulkan:clear-value))
  (frame-index :uint32)
  (image-count :uint32)
  (semaphore-count :uint32)
  (semaphore-index :uint32)
  (frames (:pointer (:struct imgui-impl-vulkan-h-frame)))
  (frame-semaphores (:pointer (:struct imgui-impl-vulkan-h-frame-semaphores))))

(cffi:defcfun ("ImGui_ImplVulkan_Init" imgui-impl-vulkan-init) :bool
  (info (:pointer (:struct imgui-impl-vulkan-init-info))))

(cffi:defcfun ("ImGui_ImplVulkan_Shutdown" imgui-impl-vulkan-shutdown) :void)

(cffi:defcfun ("ImGui_ImplVulkan_NewFrame" imgui-impl-vulkan-new-frame) :void)

(cffi:defcfun ("ImGui_ImplVulkan_RenderDrawData" imgui-impl-vulkan-render-draw-data) :void
  (draw-data :pointer)
  (command-buffer vulkan:command-buffer)
  (pipeline vulkan:pipeline))

(cffi:defcfun ("ImGui_ImplVulkan_CreateFontsTexture" imgui-impl-vulkan-create-fonts-texture) :bool)

(cffi:defcfun ("ImGui_ImplVulkan_DestroyFontsTexture" imgui-impl-vulkan-destroy-fonts-texture) :void)

(cffi:defcfun ("ImGui_ImplVulkan_SetMinImageCount" imgui-impl-vulkan-set-min-image-count) :void
  (min-image-count :uint32))

(cffi:defcfun ("ImGui_ImplVulkan_AddTexture" imgui-impl-vulkan-add-texture) vulkan:descriptor-set
  (sampler vulkan:sampler)
  (image-view vulkan:image-view)
  (image-layout vulkan:image-layout))

(cffi:defcfun ("ImGui_ImplVulkan_RemoveTexture" imgui-impl-vulkan-remove-texture) :void
  (descriptor-set vulkan:descriptor-set))

(cffi:defcfun ("ImGui_ImplVulkan_LoadFunctions" imgui-impl-vulkan-load-functions) :bool
  (loader-func :pointer)
  (user-data :pointer))

(cffi:defcfun ("ImGui_ImplVulkanH_CreateOrResizeWindow" imgui-impl-vulkan-h-create-or-resize-window) :void
  (instance vulkan:instance)
  (physical-device vulkan:physical-device)
  (device vulkan:device)
  (wnd (:pointer (:struct imgui-impl-vulkan-h-window)))
  (queue-family :uint32)
  (allocator :pointer)
  (w :int)
  (h :int)
  (min-image-count :uint32))

(cffi:defcfun ("ImGui_ImplVulkanH_DestroyWindow" imgui-impl-vulkan-h-destroy-window) :void
  (instance vulkan:instance)
  (device vulkan:device)
  (wnd (:pointer (:struct imgui-impl-vulkan-h-window)))
  (allocator :pointer))

(cffi:defcfun ("ImGui_ImplVulkanH_SelectSurfaceFormat" imgui-impl-vulkan-h-select-surface-format) (:struct vulkan:surface-format-khr)
  (physical-device vulkan:physical-device)
  (surface vulkan:surface-khr)
  (request-formats :pointer)
  (request-formats-count :int)
  (request-color-space vulkan:color-space-khr))

(cffi:defcfun ("ImGui_ImplVulkanH_SelectPresentMode" imgui-impl-vulkan-h-select-present-mode) vulkan:present-mode-khr
  (physical-device vulkan:physical-device)
  (surface vulkan:surface-khr)
  (request-modes (:pointer vulkan:present-mode-khr))
  (request-modes-count :int))

(cffi:defcfun ("ImGui_ImplVulkanH_GetMinImageCountFromPresentMode" imgui-impl-vulkan-h-get-min-image-count-from-present-mode) :int
  (present-mode vulkan:present-mode-khr))


(cffi:defcfun ("ImGui_ImplSDL2_InitForVulkan" imgui-impl-sdl2-init-for-vulkan) :bool
  (window :pointer))


(cffi:defcfun ("SDL_Vulkan_GetInstanceExtensions" sdl-vulkan-get-instance-extensions) vulkan:bool32
  (window :pointer)
  (count :pointer)
  (names :pointer))

(cffi:defcfun ("SDL_Vulkan_CreateSurface" sdl-vulkan-create-surface) vulkan:bool32
  (window :pointer)
  (instance vulkan:instance)
  (surface vulkan:surface-khr))



;; static VkInstance               g_Instance = VK_NULL_HANDLE;
(defvar *instance*)
;; static VkPhysicalDevice         g_PhysicalDevice = VK_NULL_HANDLE;
(defvar *physical-device*)
;; static VkDevice                 g_Device = VK_NULL_HANDLE;
(defvar *device*)
;; static uint32_t                 g_QueueFamily = (uint32_t)-1;
(defvar *queue-family* #xffffffff)
;; static VkQueue                  g_Queue = VK_NULL_HANDLE;
(defvar *queue* (cffi:foreign-alloc :pointer))
;; static VkDebugReportCallbackEXT g_DebugReport = VK_NULL_HANDLE;
(defvar *debug-report*)
;; static VkPipelineCache          g_PipelineCache = VK_NULL_HANDLE;
(defvar *pipeline-cache* (cffi:foreign-alloc :pointer))
;; static VkDescriptorPool         g_DescriptorPool = VK_NULL_HANDLE;
(defvar *descriptor-pool*)

;; static ImGui_ImplVulkanH_Window g_MainWindowData;
(defvar *main-window-data* (cffi:foreign-alloc '(:struct imgui-impl-vulkan-h-window)))
;; static uint32_t                 g_MinImageCount = 2;
(defparameter *min-image-count* 2)
;; static bool                     g_SwapChainRebuild = false;
(defparameter *swap-chain-rebuild* nil)

(cffi:defcallback check-vk-result :void
    ((err vulkan:result))
  (unless (eq err :success)
    (format t "[vulkan] Error: VkResult = ~d!" err)
    (unless (member err '(:success :not-ready :timeout :event-set :event-reset :incomplete
                          :suboptimal-khr
                          :thread-idle-khr
                          :thread-done-khr
                          :operation-deferred-khr
                          :operation-not-deferred-khr
                          :pipeline-compile-required-ext))
      (error "[vulkan] Error: VkResult = ~d!" err))))

(defun setup-vulkan (extensions extensions-count)
  (let* ((enabled-extension-names
           (loop for i below extensions-count
                 collect (cffi:foreign-string-to-lisp
                          (cffi:mem-aref extensions :pointer i))))
         (instance-create-info
           (vk:make-instance-create-info
            :enabled-extension-names enabled-extension-names)))
    (setf *instance* (vk:create-instance instance-create-info))

    (loop for device in (vk:enumerate-physical-devices *instance*)
          if (eq (vk:device-type (vk:get-physical-device-properties device))
                 :discrete-gpu)
            do (setf *physical-device* device)
               (loop-finish))

    (loop for queue in (vk:get-physical-device-queue-family-properties *physical-device*)
          for i from 0
          if (member :graphics (vk:queue-flags queue))
            do (setf *queue-family* i)
               (loop-finish))

    (let* ((queue-info (vk:make-device-queue-create-info
                        :queue-family-index *queue-family*
                        :queue-priorities '(1.0)))
           (create-info (vk:make-device-create-info
                         :queue-create-infos (list queue-info)
                         :enabled-extension-names '("VK_KHR_swapchain"))))
      (setf *device* (vk:create-device *physical-device* create-info)))

    (let ((pool-info (vk:make-descriptor-pool-create-info
                      :flags '(:free-descriptor-set)
                      :max-sets 1
                      :pool-sizes (list (vk:make-descriptor-pool-size
                                         :type :combined-image-sampler
                                         :descriptor-count 1)))))
      (setf *descriptor-pool* (vk:create-descriptor-pool *device* pool-info)))))

(defun setup-vulkan-window (surface width height)
  (cffi:with-foreign-slots ((clear-enable present-mode)
                            *main-window-data*
                            (:struct imgui-impl-vulkan-h-window))
    (setf clear-enable t)

    (unless (vk:get-physical-device-surface-support-khr
             *physical-device* *queue-family* (vk:make-surface-khr-wrapper surface))
      (error "Error no WSI support on physical device 0!"))

    ;; FIXME VkFormat はどれ？
    (cffi:with-foreign-object (request-format :int 4)
      (let ((VK_FORMAT_B8G8R8A8_UNORM 44)
            (VK_COLORSPACE_SRGB_NONLINEAR_KHR 0))
        (loop for i below 4
              do (setf (cffi:mem-aref request-format :int i)
                       VK_FORMAT_B8G8R8A8_UNORM))
        (let ((ret (imgui-impl-vulkan-h-select-surface-format
                    (vk:raw-handle *physical-device*) surface request-format 4
                    VK_COLORSPACE_SRGB_NONLINEAR_KHR)))
          (setf (cffi:mem-ref *main-window-data* '(:struct imgui-impl-vulkan-h-window))
                `(surface-format ,ret)))))

    (cffi:with-foreign-object (present-modes 'vulkan:present-mode-khr)
      (let ((VK_PRESENT_MODE_FIFO_KHR 2))
        (setf (cffi:mem-aref present-modes 'vulkan:present-mode-khr 0)
              VK_PRESENT_MODE_FIFO_KHR)
        (setf present-mode (imgui-impl-vulkan-h-select-present-mode
                            (vk:raw-handle *physical-device*) surface present-modes 1))))

    (imgui-impl-vulkan-h-create-or-resize-window
     (vk:raw-handle *instance*) (vk:raw-handle *physical-device*)
     (vk:raw-handle *device*) *main-window-data*
     *queue-family* vk:*default-allocator*
     width height *min-image-count*)))

(defun semaphore (which)
  (cffi:with-foreign-slots ((frame-semaphores semaphore-index)
                            *main-window-data* (:struct imgui-impl-vulkan-h-window))
    (let ((frame-semaphore (cffi:mem-aref frame-semaphores
                                          '(:struct imgui-impl-vulkan-h-frame-semaphores)
                                          semaphore-index)))
      (cffi:foreign-slot-value
       frame-semaphore
       '(:struct imgui-impl-vulkan-h-frame-semaphores)
       which))))

(defun frame-render (draw-data)
  (cffi:with-foreign-slots ((swapchain frame-index
                             frames render-pass width height clear-value)
                            *main-window-data* (:struct imgui-impl-vulkan-h-window))
    (let* ((image-acquired-semaphore (semaphore 'image-acquired-semaphore))
           (render-complete-semaphore (semaphore 'render-complete-semaphore)))

      (handler-case
          (setf frame-index (vk:acquire-next-image-khr
                             *device* swapchain +uint64-max+ image-acquired-semaphore))
        ((or %vk:error-out-of-date-khr %vk:suboptimal-khr) ()
          (setf *swap-chain-rebuild* nil)
          (return-from frame-render)))

      (let* ((fd (cffi:mem-aref frames '(:pointer (:struct imgui-impl-vulkan-h-frame))
                                frame-index)))
        (cffi:with-foreign-slots ((fence command-pool command-buffer framebuffer)
                                  fd (:struct imgui-impl-vulkan-h-frame))
          (vk:wait-for-fences *device* (list fence) vk:+true+ +uint64-max+)
          (vk:reset-fences *device* (list fence))

          (vk:reset-command-pool *device* command-pool)

          (vk:begin-command-buffer
           command-buffer
           (vk:make-command-buffer-begin-info :flags '(:one-time-submit)))

          (vk:cmd-begin-render-pass
           command-buffer
           (vk:make-render-pass-begin-info
            :render-pass render-pass
            :framebuffer framebuffer
            :render-area (vk:make-rect-2d :extent (vk:make-extent-2d :width width :height height))
            :clear-values (list clear-value))
           :inline)

          (imgui-impl-vulkan-render-draw-data draw-data command-buffer (cffi:null-pointer))

          (vk:cmd-end-render-pass command-buffer)
          (vk:end-command-buffer command-buffer)

          (vk:queue-submit
           *queue*
           (list (vk:make-submit-info
                  :wait-semaphores (list image-acquired-semaphore)
                  :wait-dst-stage-mask '(:color-attachment-output)
                  :command-buffers (list command-buffer)
                  :signal-semaphores (list render-complete-semaphore)))
           fence))))))

(defun frame-present ()
  (unless *swap-chain-rebuild*
    (cffi:with-foreign-slots ((swapchain frame-index semaphore-index image-count)
                              *main-window-data*
                              (:struct imgui-impl-vulkan-h-window))
      (handler-case
          (vk:queue-present-khr
           *queue*
           (vk:make-present-info-khr
            :wait-semaphores (list (semaphore 'render-complete-semaphore))
            :swapchains (list swapchain)
            :image-indices (list frame-index)))
        ((or %vk:error-out-of-date-khr %vk:suboptimal-khr) ()
          (setf *swap-chain-rebuild* t)
          (return-from frame-present)))

      (setf semaphore-index (mod (1+ semaphore-index) image-count)))))

(defun cleanup-vulkan-window ()
  (imgui-impl-vulkan-h-destroy-window
   (vk:raw-handle *instance*) (vk:raw-handle *device*)
   *main-window-data* vk:*default-allocator*))

(defun cleanup-vulkan ()
  (vk:destroy-descriptor-pool *device* *descriptor-pool*)
  (vk:destroy-device *device*)
  (vk:destroy-instance *instance*))

(defun vulkan-backend-main ()

  (loop for i below (cffi:foreign-type-size '(:struct imgui-impl-vulkan-h-window))
        do (setf (cffi:mem-ref *main-window-data* :char i) 0))
  (setf (cffi:mem-ref *queue* :pointer) (cffi:null-pointer))
  (setf (cffi:mem-ref *pipeline-cache* :pointer) (cffi:null-pointer))

  (sdl2:init sdl2-ffi:+sdl-init-video+ sdl2-ffi:+sdl-init-timer+)
  (let ((window (sdl2:create-window :title "DGW" :w 1024 :h 768
                                    :flags (list sdl2-ffi:+sdl-window-shown+
                                                 sdl2-ffi:+sdl-window-vulkan+
                                                 sdl2-ffi:+sdl-window-resizable+
                                                 sdl2-ffi:+sdl-window-allow-highdpi+ ))))

    (unwind-protect
         (cffi:with-foreign-slots ((surface render-pass image-count)
                                   *main-window-data*
                                   (:struct imgui-impl-vulkan-h-window))
           (cffi:with-foreign-object (extensions-count :uint32)
             (sdl-vulkan-get-instance-extensions (autowrap:ptr window)
                                                 extensions-count
                                                 (cffi:null-pointer))
             (cffi:with-foreign-object (extensions :pointer (cffi:mem-ref extensions-count :uint32))
               (sdl-vulkan-get-instance-extensions (autowrap:ptr window)
                                                   extensions-count
                                                   extensions)
               (setup-vulkan extensions (cffi:mem-ref extensions-count :uint32))))

           (unless (sdl-vulkan-create-surface (autowrap:ptr window) (vk:raw-handle *instance*)
                                              (cffi:foreign-slot-pointer *main-window-data*
                                                                         '(:struct imgui-impl-vulkan-h-window)
                                                                         'surface))
             (error "Failed to create Vulkan surface!"))

           (multiple-value-bind (w h) (sdl2:get-window-size window)
             (setup-vulkan-window surface w h))

           (ig:create-context (cffi:null-pointer))

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
             (autowrap:with-alloc (glyph-ranges 'ig:im-wchar 3)
               (setf (plus-c:c-ref glyph-ranges ig:im-wchar 0) #x0020
                     (plus-c:c-ref glyph-ranges ig:im-wchar 1) #xfffd
                     (plus-c:c-ref glyph-ranges ig:im-wchar 2) 0)
               (ig:im-font-atlas-add-font-from-file-ttf
                (plus-c:c-ref io ig:im-gui-io :fonts)
                (namestring (merge-pathnames "factory/font/NotoSansJP-Regular.ttf" dgw::*working-directory*))
                16.0
                (cffi:null-pointer)
                glyph-ranges)))

           (imgui-impl-sdl2-init-for-vulkan (autowrap:ptr window))

           (cffi:with-foreign-object (init-info '(:struct imgui-impl-vulkan-init-info))
             (let ((wd-render-pass render-pass)
                   (wd-image-count image-count))
               (cffi:with-foreign-slots ((instance
                                          physical-device
                                          device
                                          queue-family
                                          queue
                                          pipeline-cache
                                          descriptor-pool
                                          render-pass
                                          subpass
                                          min-image-count
                                          image-count
                                          msaa-samples
                                          allocator
                                          check-vk-result-fn) init-info
                                         (:struct imgui-impl-vulkan-init-info))
                 (setf instance (vk:raw-handle *instance*))
                 (setf physical-device (vk:raw-handle *physical-device*))
                 (setf device (vk:raw-handle *device*))
                 (setf queue-family *queue-family*)
                 (setf queue *queue*)
                 (setf pipeline-cache *pipeline-cache*)
                 (setf descriptor-pool (vk:raw-handle *descriptor-pool*))
                 (setf render-pass wd-render-pass)
                 (setf subpass 0)
                 (setf min-image-count *min-image-count*)
                 (setf image-count wd-image-count)
                 (setf msaa-samples :1)
                 (setf allocator vk:*default-allocator*)
                 (setf check-vk-result-fn (cffi:callback check-vk-result))
                 ;; FIXMI ここで落ちる
                 ;;     VkResult err = vkCreateGraphicsPipelines(device, pipelineCache, 1, &info, allocator, pipeline);
                 (imgui-impl-vulkan-init init-info))))

           (setf dgw::*done* nil)
           (sdl2:with-sdl-event (e)
             (loop until dgw::*done* do
               (handler-bind ((error 'error-handler))
                 (vulkan-ui-loop window e)))))

      (vk:device-wait-idle *device*)
      (imgui-impl-vulkan-shutdown)
      (ig-backend::impl-sdl2-shutdown)
      (ig:destroy-context (cffi:null-pointer))
      (cleanup-vulkan-window)
      (cleanup-vulkan)
      (sdl2:destroy-window window)
      (sdl2:quit))))

(defun vulkan-ui-loop (window e)
  (loop while (/= (sdl2-ffi.functions:sdl-poll-event e) 0)
        do (ig-backend::impl-sdl2-process-event (autowrap:ptr e))
           (if (eq (sdl2:get-event-type e) :quit)
               (setf dgw::*done* t)
               (if (and (eq (sdl2:get-event-type e) :windowevent)
                        (sdl2::c-let ((event sdl2-ffi:sdl-event :from e))
                          (sdl2::c-let ((we sdl2-ffi:sdl-window-event :from (event :window)))
                            (and (= (we :event) sdl2-ffi::+sdl-windowevent-close+)
                                 (= (we :window-id) (sdl2:get-window-id window))))))
                   (setf dgw::*done* t))))
  (when *swap-chain-rebuild*
    (multiple-value-bind (width height) (sdl2:get-window-size window)
      (when (and (plusp width) (plusp height))
        (imgui-impl-vulkan-set-min-image-count *min-image-count*)
        (imgui-impl-vulkan-h-create-or-resize-window
         (vk:raw-handle *instance*) (vk:raw-handle *physical-device*)
         (vk:raw-handle *device*) *main-window-data*
         *queue-family* vk:*default-allocator*
         width height *min-image-count*)
        (setf (cffi:foreign-slot-value *main-window-data*
                                       '(:struct imgui-impl-vulkan-h-window)
                                       'frame-index)
              0)
        (setf *swap-chain-rebuild* nil))))

  (imgui-impl-vulkan-new-frame)
  (ig-backend::impl-sdl2-new-frame)
  (ig::new-frame)

  (ig:show-demo-window (cffi:null-pointer))

  (ig::render)

  (let* ((draw-data (ig:get-draw-data))
         (minimized-p (or (<= (plus-c:c-ref draw-data ig:im-draw-data :display-size :x) 0.0)
                          (<= (plus-c:c-ref draw-data ig:im-draw-data :display-size :y) 0.0))))
    (unless minimized-p
      (frame-render draw-data)
      (frame-present))))

(in-package :vulkan-backend)

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
(defvar *queue*)
;; static VkDebugReportCallbackEXT g_DebugReport = VK_NULL_HANDLE;
(defvar *debug-report*)
;; static VkDescriptorPool         g_DescriptorPool = VK_NULL_HANDLE;
(defvar *descriptor-pool*)

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
         (enabled-extension-names
           (print (cons VULKAN:+KHR-GET-PHYSICAL-DEVICE-PROPERTIES-2-EXTENSION-NAME+ enabled-extension-names)))
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
      (setf *device* (vk:create-device *physical-device* create-info))
      (setf *queue* (vk:get-device-queue *device* *queue-family* 0)))

    (let* ((pool-sizes (list (vk:make-descriptor-pool-size
                              :type :sampler :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :combined-image-sampler :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :sampled-image :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :storage-image :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :uniform-texel-buffer :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :storage-texel-buffer :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :uniform-buffer :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :storage-buffer :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :uniform-buffer-dynamic :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :storage-buffer-dynamic :descriptor-count 1000)
                             (vk:make-descriptor-pool-size
                              :type :input-attachment :descriptor-count 1000)))
           (pool-info (vk:make-descriptor-pool-create-info
                       :flags '(:free-descriptor-set)
                       :max-sets (* 1000 (length pool-sizes))
                       :pool-sizes pool-sizes)))
      (setf *descriptor-pool* (vk:create-descriptor-pool *device* pool-info)))))

(defun setup-vulkan-window (main-window-data surface width height)
  (cffi:with-foreign-slots ((clear-enable present-mode)
                            main-window-data
                            (:struct imgui-impl-vulkan-h-window))
    (setf clear-enable t)
    (setf (cffi:foreign-slot-value
           main-window-data
           '(:struct imgui-impl-vulkan-h-window)
           'surface)
          surface)

    (unless (vk:get-physical-device-surface-support-khr
             *physical-device* *queue-family* (vk:make-surface-khr-wrapper surface))
      (error "Error no WSI support on physical device 0!"))

    ;; FIXME VkFormat はどれ？
    (cffi:with-foreign-object (request-format :int 4)
      (let ((VK_FORMAT_B8G8R8A8_UNORM 44)
            (VK_FORMAT_R8G8B8A8_UNORM 37)
            (VK_FORMAT_B8G8R8_UNORM 30)
            (VK_FORMAT_R8G8B8_UNORM 23)
            (VK_COLORSPACE_SRGB_NONLINEAR_KHR 0))
        (loop for i below 4
              for v in (list VK_FORMAT_B8G8R8A8_UNORM VK_FORMAT_R8G8B8A8_UNORM
                             VK_FORMAT_B8G8R8_UNORM VK_FORMAT_R8G8B8_UNORM)
              do (setf (cffi:mem-aref request-format :int i)
                       v))
        (let ((ret (imgui-impl-vulkan-h-select-surface-format
                    (vk:raw-handle *physical-device*) surface request-format 4
                    VK_COLORSPACE_SRGB_NONLINEAR_KHR)))
          (setf (cffi:mem-ref main-window-data '(:struct imgui-impl-vulkan-h-window))
                `(surface-format ,ret)))))

    (cffi:with-foreign-object (present-modes 'vulkan:present-mode-khr)
      (let ((VK_PRESENT_MODE_FIFO_KHR 2))
        (setf (cffi:mem-aref present-modes 'vulkan:present-mode-khr 0)
              VK_PRESENT_MODE_FIFO_KHR)
        (setf present-mode (imgui-impl-vulkan-h-select-present-mode
                            (vk:raw-handle *physical-device*) surface present-modes 1))))

    (imgui-impl-vulkan-h-create-or-resize-window
     (vk:raw-handle *instance*) (vk:raw-handle *physical-device*)
     (vk:raw-handle *device*) main-window-data
     *queue-family* vk:*default-allocator*
     width height *min-image-count*)
    #+nil
    (break "after first imgui-impl-vulkan-h-create-or-resize-window ~a"
           (cffi:foreign-slot-value main-window-data '(:struct imgui-impl-vulkan-h-window)
                                    'image-count))))

(defun semaphore (main-window-data which)
  (cffi:with-foreign-slots ((frame-semaphores semaphore-index)
                            main-window-data (:struct imgui-impl-vulkan-h-window))
    (let ((frame-semaphore (cffi:mem-aref frame-semaphores
                                          '(:struct imgui-impl-vulkan-h-frame-semaphores)
                                          semaphore-index)))
      (vk:make-semaphore-wrapper (getf frame-semaphore which)))))

(defun frame-render (main-window-data draw-data)
  (cffi:with-foreign-slots ((swapchain frame-index
                                       frames render-pass width height clear-value)
                            main-window-data (:struct imgui-impl-vulkan-h-window))
    (let* ((image-acquired-semaphore (semaphore main-window-data 'image-acquired-semaphore))
           (render-complete-semaphore (semaphore main-window-data 'render-complete-semaphore)))

      (handler-case
          (setf frame-index (vk:acquire-next-image-khr
                             *device* (vk:make-swapchain-khr-wrapper swapchain) +uint64-max+
                             image-acquired-semaphore))
        ((or %vk:error-out-of-date-khr %vk:suboptimal-khr) ()
          (setf *swap-chain-rebuild* nil)
          (return-from frame-render)))

      (let* ((fd (cffi:mem-aref frames '(:struct imgui-impl-vulkan-h-frame)
                                frame-index)))
        (let ((fence (vk:make-fence-wrapper (getf fd 'fence)))
              (command-pool (vk:make-command-pool-wrapper (getf fd 'command-pool)))
              (command-buffer (vk:make-command-buffer-wrapper (getf fd 'command-buffer)))
              (framebuffer (vk:make-framebuffer-wrapper (getf fd 'framebuffer))))
          (vk:wait-for-fences *device* (list fence) t +uint64-max+)
          (vk:reset-fences *device* (list fence))

          (vk:reset-command-pool *device* command-pool)

          (vk:begin-command-buffer
           command-buffer
           (vk:make-command-buffer-begin-info :flags '(:one-time-submit)))

          (vk:cmd-begin-render-pass
           command-buffer
           (vk:make-render-pass-begin-info
            :render-pass (vk:make-render-pass-wrapper render-pass)
            :framebuffer framebuffer
            :render-area (vk:make-rect-2d :extent (vk:make-extent-2d :width width :height height))
            ;; TODO imgui-impl-vulkan-h-window の clear-value を使う
            :clear-values (list (vk:make-clear-value
                                 :color (vk:make-clear-color-value
                                         :float-32 #(0.45 0.55 0.60 0.80)))))
           :inline)

          (imgui-impl-vulkan-render-draw-data
           (autowrap:ptr draw-data) (vk:raw-handle command-buffer) (cffi:null-pointer))

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

(defun frame-present (main-window-data)
  (unless *swap-chain-rebuild*
    (cffi:with-foreign-slots ((swapchain frame-index semaphore-index semaphore-count)
                              main-window-data
                              (:struct imgui-impl-vulkan-h-window))
      (handler-case
          (vk:queue-present-khr
           *queue*
           (vk:make-present-info-khr
            :wait-semaphores (list (semaphore main-window-data 'render-complete-semaphore))
            :swapchains (list (vk:make-swapchain-khr-wrapper swapchain))
            :image-indices (list frame-index)))
        ((or %vk:error-out-of-date-khr %vk:suboptimal-khr) ()
          (setf *swap-chain-rebuild* t)
          (return-from frame-present)))

      (setf semaphore-index (mod (1+ semaphore-index) semaphore-count)))))

(defun cleanup-vulkan-window (main-window-data)
  (imgui-impl-vulkan-h-destroy-window
   (vk:raw-handle *instance*) (vk:raw-handle *device*)
   main-window-data vk:*default-allocator*))

(defun cleanup-vulkan ()
  (vk:destroy-descriptor-pool *device* *descriptor-pool*)
  (vk:destroy-device *device*)
  (vk:destroy-instance *instance*))

(defmethod utaticl.core:run-with-backend (app (backend (eql :sdl-vulkan)))
  (cffi:with-foreign-objects ((main-window-data '(:struct imgui-impl-vulkan-h-window))
                              (surface 'vulkan:surface-khr))
    (ftw:memset main-window-data (cffi:foreign-type-size '(:struct imgui-impl-vulkan-h-window)))
    (ftw:memset surface (cffi:foreign-type-size 'vulkan:surface-khr))

    (autowrap:with-alloc (glyph-ranges 'ig:im-wchar 3)
      (sdl2:with-init (:video :timer)
        (sdl2:with-window (window :title "UTATICL"
                                  :x 10 :y 40
                                  :w 1600 :h 1200
                                  :flags '(:vulkan :resizable :allow-highdpi))
          ;; 一度 hide しないと表示されない
          (sdl2:hide-window window)
          (sdl2:show-window window)
          ;; (sdl2:raise-window window) 効果ない・・・

          (autowrap:with-alloc (wm-info 'sdl2-ffi:sdl-syswm-info)
            (sdl2-ffi.functions:sdl-get-version (plus-c:c-ref wm-info sdl2-ffi:sdl-syswm-info :version plus-c:&))
            (sdl2-ffi.functions:sdl-get-window-wm-info window wm-info)
            ;; なんで :win :windows がないの？ しかたないので :x11 :display を使う
            ;; たぶんメモリレイアウト的に大丈夫なはず
            (setf utaticl.core:*hwnd* (plus-c:c-ref wm-info sdl2-ffi:sdl-syswm-info :info :x11 :display)))

          (unwind-protect
               (cffi:with-foreign-slots ((render-pass image-count)
                                         main-window-data
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
                                                    surface)
                   (error "Failed to create Vulkan surface!"))

                 (multiple-value-bind (w h) (sdl2:get-window-size window)
                   (setup-vulkan-window main-window-data
                                        (cffi:mem-ref surface 'vulkan:surface-khr)
                                        w h))

                 (ig:create-context (cffi:null-pointer))

                 (let ((io (ig:get-io)))
                   (ensure-directories-exist (merge-pathnames "user/config/" utaticl.core:*working-directory*))
                   (setf (plus-c:c-ref io ig:im-gui-io :ini-filename)
                         (namestring (merge-pathnames "user/config/imgui.ini" utaticl.core:*working-directory*)))
                   (setf (plus-c:c-ref io ig:im-gui-io :config-docking-with-shift) 1)
                   (setf (plus-c:c-ref io ig:im-gui-io :config-windows-move-from-title-bar-only) 1)
                   (setf (plus-c:c-ref io ig:im-gui-io :config-flags)
                         (logior (plus-c:c-ref io ig:im-gui-io :config-flags)
                                 ig:+im-gui-config-flags-nav-enable-keyboard+
                                 ig:+im-gui-config-flags-docking-enable+))

                   ;; (ig:im-font-atlas-add-font-default
                   ;;  (plus-c:c-ref io ig:im-gui-io :fonts)
                   ;;  (cffi:null-pointer))
                   (let ((font (namestring (merge-pathnames "factory/font/NotoSansJP-Regular.ttf"
                                                            utaticl.core:*working-directory*))))
                     (setf (plus-c:c-ref glyph-ranges ig:im-wchar 0) #x0020
                           (plus-c:c-ref glyph-ranges ig:im-wchar 1) #xfffd
                           (plus-c:c-ref glyph-ranges ig:im-wchar 2) 0)
                     (ig:im-font-atlas-add-font-from-file-ttf
                      (plus-c:c-ref io ig:im-gui-io :fonts) font 16.0 (cffi:null-pointer) glyph-ranges)
                     (log:info "Load ~A" font)))

                 (ig:style-colors-dark (cffi:null-pointer))

                 (imgui-impl-sdl2-init-for-vulkan (autowrap:ptr window))

                 (cffi:with-foreign-object (init-info '(:struct imgui-impl-vulkan-init-info))
                   (loop for i below (cffi:foreign-type-size '(:struct imgui-impl-vulkan-init-info))
                         do (setf (cffi:mem-ref init-info :char i) 0))

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
                                                use-dynamic-rendering
                                                allocator
                                                check-vk-result-fn
                                                min-allocation-size) init-info
                                               (:struct imgui-impl-vulkan-init-info))
                       (setf instance (vk:raw-handle *instance*))
                       (setf physical-device (vk:raw-handle *physical-device*))
                       (setf device (vk:raw-handle *device*))
                       (setf queue-family *queue-family*)
                       (setf queue (vk:raw-handle *queue*))
                       (setf pipeline-cache (cffi:null-pointer))
                       (setf descriptor-pool (vk:raw-handle *descriptor-pool*))
                       (setf render-pass wd-render-pass)
                       (setf subpass 0)
                       (setf min-image-count *min-image-count*)
                       (setf image-count wd-image-count)
                       (setf msaa-samples :1)
                       (setf allocator vk:*default-allocator*)
                       (setf check-vk-result-fn (cffi:callback check-vk-result))
                       ;; (setf use-dynamic-rendering nil)
                       ;; (setf min-allocation-size 0)
                       (imgui-impl-vulkan-init init-info))))

                 (setf utaticl.core:*done* nil)
                 (setf (utaticl.core:.window app) window)
                 (dd-ffi::with-drag-drop-handler (utaticl.core:*hwnd*)
                   (unwind-protect
                        (sdl2:with-sdl-event (e)
                          (loop until utaticl.core:*done* do
                            (vulkan-ui-loop main-window-data app window e)
                                #+nil
                                 (handler-case
                                     (vulkan-ui-loop main-window-data app window e)
                                   (error (e)
                                     (log4cl:log-error "Error ~a!~%~a" e
                                                       (with-output-to-string (out)
                                                         (sb-debug:print-backtrace :stream out))))))))))

            (vk:device-wait-idle *device*)
            (imgui-impl-vulkan-shutdown)
            (ig-backend::impl-sdl2-shutdown)
            (ig:destroy-context (cffi:null-pointer))
            (cleanup-vulkan-window main-window-data)
            (cleanup-vulkan)))))))

(defun vulkan-ui-loop (main-window-data app window e)
  (loop while (/= (sdl2-ffi.functions:sdl-poll-event e) 0)
        do (ig-backend::impl-sdl2-process-event (autowrap:ptr e))
           (if (eq (sdl2:get-event-type e) :quit)
               (setf utaticl.core:*done* t)
               (if (and (eq (sdl2:get-event-type e) :windowevent)
                        (sdl2::c-let ((event sdl2-ffi:sdl-event :from e))
                          (sdl2::c-let ((we sdl2-ffi:sdl-window-event :from (event :window)))
                            (and (= (we :event) sdl2-ffi::+sdl-windowevent-close+)
                                 (= (we :window-id) (sdl2:get-window-id window))))))
                   (setf utaticl.core:*done* t))))

  (multiple-value-bind (width height) (sdl2:get-window-size window)
    (when (and (plusp width) (plusp height)
               (or *swap-chain-rebuild*
                   (/= (cffi:foreign-slot-value main-window-data
                                                '(:struct imgui-impl-vulkan-h-window)
                                                'width)
                       width)
                   (/= (cffi:foreign-slot-value main-window-data
                                                '(:struct imgui-impl-vulkan-h-window)
                                                'height)
                       height)))
      (imgui-impl-vulkan-set-min-image-count *min-image-count*)
      (imgui-impl-vulkan-h-create-or-resize-window
       (vk:raw-handle *instance*) (vk:raw-handle *physical-device*)
       (vk:raw-handle *device*) main-window-data
       *queue-family* vk:*default-allocator*
       width height *min-image-count*)
      (setf (cffi:foreign-slot-value main-window-data
                                     '(:struct imgui-impl-vulkan-h-window)
                                     'frame-index)
            0)
      (setf *swap-chain-rebuild* nil)))

  (imgui-impl-vulkan-new-frame)
  (ig-backend::impl-sdl2-new-frame)
  (ig::new-frame)

  (utaticl.core:with-debugger
    (progn
        (ig:show-demo-window (cffi:null-pointer))
        (let ((utaticl.core:*render-context* (make-instance 'utaticl.core:render-context)))
          (utaticl.core:render app)
          (utaticl.core:cmd-run app))))

  (ig::render)

  (let* ((draw-data (ig:get-draw-data))
         (minimized-p (or (<= (plus-c:c-ref draw-data ig:im-draw-data :display-size :x) 0.0)
                          (<= (plus-c:c-ref draw-data ig:im-draw-data :display-size :y) 0.0))))
    (unless minimized-p
      (let ((clear-value (cffi:foreign-slot-value main-window-data
                                                  '(:struct imgui-impl-vulkan-h-window)
                                                  'clear-value)))
        (setf clear-value (vk:make-clear-value
                           :color (vk:make-clear-color-value
                                   :float-32 #(0.45 0.55 0.60 0.80)))))
      (frame-render main-window-data draw-data)
      (frame-present main-window-data))))

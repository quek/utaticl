(in-package :utaticl.core)

(defmethod initialize-instance :before ((self app) &key)
  (clrhash *neko-map*)
  (dd-reset)
  (setf *config* (make-instance 'config))
  (config-load *config*)
  (setf *theme* (make-instance 'theme))
  (config-load *theme*)
  (setf *report-window* (make-instance 'report-window))

  (setf (.projects self) (list (make-instance 'project)))
  )

(defmethod audio-device-close ((app app))
  (sb-concurrency:send-message (.audio-thread-mailbox app)
                               :close))

(defmethod audio-device-open ((app app))
  (sb-concurrency:send-message (.audio-thread-mailbox app)
                               :open))

(defmethod audio-device-start ((app app))
  (sb-concurrency:send-message (.audio-thread-mailbox app)
                               :start))

(defmethod audio-device-stop ((app app))
  (sb-concurrency:send-message (.audio-thread-mailbox app)
                               :stop))

(defmethod audio-thread-start ((app app))
  (setf (.audio-thread app)
        (sb-thread:make-thread
         (lambda ()
           (audio-thread-loop))))
  (loop until (.audio-device app)
        do (sleep .001)))

(defmethod audio-thread-stop ((app app))
  (sb-concurrency:send-message (.audio-thread-mailbox app)
                               :terminate))

(defmethod cmd-run ((self app))
  (loop for project in (.projects self)
        do (let ((*project* project))
             (cmd-run project))))

(defmethod drag-enter ((self app) files)
  (dd-start-force self files))

(defmethod drop ((app app))
  (dd-drop-did))

(defmethod render :around ((self app))
  (call-next-method)
  (when (or (not (.processing (.audio-device self)))
            (.render-audio-device-window-p self))
    (render (.audio-device-window self))))

(defmethod render ((app app))
  (let* ((*mouse-pos* (ig:get-mouse-pos))
         (*style* (ig:get-style))
         (*scrollbar-size* (plus-c:c-ref *style* ig:im-gui-style :scrollbar-size))
         (*item-spacing-x* (plus-c:c-ref *style* ig:im-gui-style :item-spacing :x))
         (*item-spacing-y* (plus-c:c-ref *style* ig:im-gui-style :item-spacing :y)))
    (loop for project in (.projects app)
          do (let ((*project* project))
               (render project)))
    (render (.color-window app))
    (render *report-window*)
    (when (dd-src)
      (ig:with-tooltip
        (loop for src in (dd-src)
              do (dd-show src))))))

(defmethod process ((self app))
  (sb-thread:with-mutex ((.mutex self))
    (loop for project in (.projects self)
          do (process project)))
  (incf (.steady-time self) (.frames-per-buffer *config*)))

(defmethod sys-window-pos ((app app))
  (sys-window-pos% (.backend app) (.window app)))

(defmethod sys-window-pos% ((backend (eql :glfw-opengl3)) window)
  (glfw:get-window-position window))

(defmethod sys-window-pos% ((backend (eql :sdl-vulkan)) window)
  (multiple-value-list (sdl2:get-window-position window)))

(defmethod terminate ((self app) &key)
  (audio-thread-stop self)
  (loop for project in (.projects self)
        do (terminate project)))

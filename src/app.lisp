(in-package :utaticl.core)

(defmethod initialize-instance :before ((self app) &key)
  (clrhash *neko-map*)
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

(defmethod drag-enter ((app app) files)
  (setf *dd-at* (car files))
  (setf *dd-srcs* files)
  (setf (.dragging-p app) t))

(defmethod drop ((app app))
  (setf (.dragging-p app) nil))

(defmethod render :around ((self app))
  (call-next-method)
  (when (or (not (.processing (.audio-device self)))
            (.render-audio-device-window-p self))
    (render (.audio-device-window self))))

(defmethod render ((app app))
  (let ((*mouse-pos* (ig:get-mouse-pos)))
    (when (.dragging-p app)
      (ig:with-drag-drop-source (ig:+im-gui-drag-drop-flags-source-extern+)
        (ig:set-drag-drop-payload +dd-extern+)
        (ig:with-tooltip
          (loop for file in *dd-srcs*
                do (ig:text file)))))
    (loop for project in (.projects app)
          do (let ((*project* project))
               (render project)))
    (render (.color-window app))
    (render *report-window*)))

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

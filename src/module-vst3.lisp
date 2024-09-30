(in-package :utaticl.core)

(defmethod initialize-instance :after ((self module-vst3) &key)
  (load-plugin self))

;;; できたらなくしたい。load-plugin へ統合
(defmethod %initialize ((self module-vst3))
  (vst3-ffi::initialize (.component self)
                        (vst3-impl::ptr (.host-applicaiton self)))
  (vst3-ffi::initialize (.controller self)
                        (vst3-impl::ptr (.host-applicaiton self)))

  (connect-componet-controller self)

  (let ((bstream (make-instance 'vst3-impl::bstream)))
    (vst3::ensure-ok (vst3-ffi::get-state (.component self) (vst3-impl::ptr bstream)))
    (setf (vst3-impl::.cursor bstream) 0)
    (handler-case
        (progn
          (vst3::ensure-ok (vst3-ffi::set-component-state (.controller self) (vst3-impl::ptr bstream)))
          (vst3-impl::release bstream))
      (vst3::not-implemented-error ()
        ;; 無視してだいじょうぶなやつ
        )))

  (vst3-ffi::set-component-handler
   (.controller self)
   (vst3-impl::ptr (vst3-impl::.component-handler (.host-applicaiton self))))

  (let ((process (vst3::query-interface
                  (.component self) vst3-ffi::+vst-iaudio-processor-iid+))
        (audio-input-bus-count (vst3-ffi::get-bus-count
                                (.component self) sb:+vst-media-types-k-audio+
                                sb:+vst-bus-directions-k-input+))
        (audio-output-bus-count (vst3-ffi::get-bus-count
                                 (.component self) sb:+vst-media-types-k-audio+
                                 sb:+vst-bus-directions-k-output+))
        (event-input-bus-count (vst3-ffi::get-bus-count
                                (.component self) sb:+vst-media-types-k-event+
                                sb:+vst-bus-directions-k-input+))
        (event-output-bus-count (vst3-ffi::get-bus-count
                                 (.component self) sb:+vst-media-types-k-event+
                                 sb:+vst-bus-directions-k-output+)))
    (setf (.audio-processor self) process)
    (setf (.audio-input-bus-count self) audio-input-bus-count)
    (setf (.audio-output-bus-count self) audio-output-bus-count)
    (setf (.event-input-bus-count self) event-input-bus-count)
    (setf (.event-output-bus-count self) event-output-bus-count)

    (vst3::ensure-ok
     (vst3-ffi::can-process-sample-size process sb:+vst-symbolic-sample-sizes-k-sample32+))

    (vst3-ffi::set-processing (.audio-processor self) 0)
    (vst3-ffi::set-active (.component self) 0)

    (autowrap:with-alloc (setup '(:struct (sb:vst-process-setup)))
      (setf (sb:vst-process-setup.process-mode setup)
            sb:+vst-process-modes-k-realtime+)
      (setf (sb:vst-process-setup.symbolic-sample-size setup)
            sb:+vst-symbolic-sample-sizes-k-sample32+)
      (setf (sb:vst-process-setup.max-samples-per-block setup)
            1024)
      (setf (sb:vst-process-setup.sample-rate setup)
            48000.0d0)
      (vst3::ensure-ok (vst3-ffi::setup-processing process (autowrap:ptr setup))))

    (loop for (count type direction) in `((,audio-input-bus-count
                                           ,sb:+vst-media-types-k-audio+
                                           ,sb:+vst-bus-directions-k-input+)
                                          (,audio-output-bus-count
                                           ,sb:+vst-media-types-k-audio+
                                           ,sb:+vst-bus-directions-k-output+)
                                          (,event-input-bus-count
                                           ,sb:+vst-media-types-k-event+
                                           ,sb:+vst-bus-directions-k-input+)
                                          (,event-output-bus-count
                                           ,sb:+vst-media-types-k-event+
                                           ,sb:+vst-bus-directions-k-output+))
          do (loop for i below count
                   do (vst3-ffi::activate-bus (.component self)
                                              type direction i 1)))

    (setf (.process-data self)
          (make-instance 'process-data-vst3
                         :audio-input-bus-count audio-input-bus-count
                         :audio-output-bus-count audio-output-bus-count)))
  (params-prepare self)
  (params-value-changed self))

(defmethod terminate ((self module-vst3) &key)
  (when (.component self)
    (disconnect-componet-controller self)
    (let ((terminate-controller-p
            (autowrap:with-alloc (obj :pointer)
              (/= (vst3-ffi::query-interface (.component self)
                                             vst3-ffi::+vst-iedit-controller-iid+
                                             obj)
                  sb:+k-result-ok+))))
      (vst3-ffi::terminate (.component self))
      (when (and (.controller self) terminate-controller-p)
        (vst3-ffi::terminate (.controller self)))))

  (terminate (.process-data self))

  (vst3-ffi::release (.factory self))
  (vst3-impl::release (.host-applicaiton self))
  (vst3::unload-library (.library self)))

(defmethod param-changes-mbox-out ((module-vst3 module-vst3))
  (loop for (id value sample-offset) = (sb-concurrency:receive-message-no-hang
                                        (.param-changes-mbox-out module-vst3))
        while id
        do (let ((param (gethash id (.params module-vst3))))
             (setf (.value param) value)
             (value-changed-by-processor param))))

(defmethod connect-componet-controller ((self module-vst3))
  (unless (.single-component-p self)
    (handler-case
        (progn
          (setf (.connection-component self)
                (vst3::query-interface (.component self)
                                       vst3-ffi::+vst-iconnection-point-iid+))
          (setf (.connection-controller self)
                (vst3::query-interface (.controller self)
                                       vst3-ffi::+vst-iconnection-point-iid+))
          (vst3-ffi::connect (.connection-component self)
                             (vst3-walk::.ptr (.connection-controller self)))
          (vst3-ffi::connect (.connection-controller self)
                             (vst3-walk::.ptr(.connection-component self))))
      (vst3::no-interface-error ()))))

(defmethod disconnect-componet-controller ((self module-vst3))
  (unless (.single-component-p self)
    (handler-case
        (progn
          (vst3-ffi::disconnect (.connection-component self)
                                (vst3-walk::.ptr (.connection-controller self)))
          (vst3-ffi::disconnect (.connection-controller self)
                                (vst3-walk::.ptr(.connection-component self)))
          (vst3-ffi::release (.connection-component self))
          (sb-ext:cancel-finalization (.connection-component self))
          (setf (.connection-component self) nil)
          (vst3-ffi::release (.connection-controller self))
          (sb-ext:cancel-finalization (.connection-controller self))
          (setf (.connection-controller self) nil))
      (vst3::no-interface-error ()))))

(defmethod begin-edit ((self module-vst3) id)
  (declare (ignore id)))

(defmethod perform-edit ((self module-vst3) id value-normalized)
  (param-editing self id value-normalized))

(defmethod end-edit ((self module-vst3) id)
  (declare (ignore id)))

(defmethod editor-close ((self module-vst3))
  (when (.view self)
    (vst3-ffi::set-frame (.view self) (cffi:null-pointer))
    (vst3-ffi::removed (.view self))
    (vst3-ffi::release (.view self))
    (setf (.view self) nil)
    (remhash (cffi:pointer-address (.hwnd self)) *hwnd-module-map*)
    (ftw:destroy-window (.hwnd self))
    (setf (.hwnd self) nil)
    t))

(defmethod editor-open ((self module-vst3))
  (let* ((view-ptr (vst3-ffi::create-view (.controller self)
                                          vst3-ffi::+vst-view-type-k-editor+))
         (view (make-instance 'vst3-ffi::iplug-view :ptr view-ptr)))
    (setf (.view self) view)
    (vst3-ffi::set-frame view (vst3-impl::ptr (vst3-impl::.plug-frame
                                               (.host-applicaiton self))))
    (autowrap:with-alloc (size '(:struct (sb:view-rect)))
      (vst3-ffi::get-size view (autowrap:ptr size))
      (let* ((resizable (= (vst3-ffi::can-resize view) sb:+k-result-true+))
             (width (- (sb:view-rect.right size)
                       (sb:view-rect.left size)))
             (height (- (sb:view-rect.bottom size)
                        (sb:view-rect.top size)))
             (hwnd (win32::make-window width height resizable)))
        (setf (.hwnd self) hwnd)
        (setf (gethash (cffi:pointer-address hwnd) *hwnd-module-map*) self)
        (vst3::ensure-ok (vst3-ffi::attached view hwnd vst3-ffi::+k-platform-type-hwnd+))
        t))))

(defmethod load-plugin ((self module-vst3))
  (with-slots (id plugin-info) self
    (when (and id (not plugin-info))
      (setf plugin-info (plugin-info-find id)))
    (when plugin-info
      (setf id (.id plugin-info))
      (let ((host-applicaiton (make-instance 'vst3-impl::host-application :module self)))
        (setf (slot-value self 'host-applicaiton) host-applicaiton))
      (let ((path (.path plugin-info)))
        (multiple-value-bind (factory library) (vst3::get-plugin-factory path)
          (let* ((component (vst3::create-component-by-id factory id))
                 (single-component-p t)
                 (controller (labels ((f ()
                                        (setf single-component-p nil)
                                        (vst3::create-instance factory
                                                               (vst3::get-controller-class-id component)
                                                               vst3-ffi::+vst-iedit-controller-iid+)))
                               (handler-case (vst3::query-interface component vst3-ffi::+vst-iedit-controller-iid+)
                                 (vst3::no-interface-error () (f))
                                 (vst3::false-error () (f))))))
            (setf (.id self) id)
            (setf (.library self) library)
            (setf (.factory self) factory)
            (setf (.component self) component)
            (setf (.controller self) controller)
            (setf (.single-component-p self) single-component-p)
            (%initialize self)))))))

(defmethod on-resize ((self module-vst3) width height)
  (when (.editor-open-p self)
    (let ((view (.view self)))
      (autowrap:with-calloc (rect '(:struct (sb:view-rect)))
        (setf (sb:view-rect.right rect) width)
        (setf (sb:view-rect.bottom rect) height)
        (vst3-ffi::on-size view (autowrap:ptr rect))))))

(defmethod param-change-add ((module-vst3 module-vst3) (param-vst3 param-vst3)
                             &optional (sample-offset 0))
  (sb-concurrency:send-message (.param-changes-mbox-in module-vst3)
                               (list (.id param-vst3)
                                     (.value param-vst3)
                                     sample-offset)))

(defmethod param-editing :after ((module-vst3 module-vst3) (param-vst3 param-vst3)
                                 value)
  (param-change-add module-vst3 param-vst3))

(defmethod params-prepare ((module-vst3 module-vst3))
  (params-clear module-vst3)
  (loop with controller = (.controller module-vst3)
        for i below (vst3-ffi::get-parameter-count controller)
        do (autowrap:with-alloc (info '(:struct (sb:vst-parameter-info)))
             (vst3-ffi::get-parameter-info controller i (autowrap:ptr info))
             (param-add module-vst3
                        (make-instance 'param-vst3 :vst-parameter-info info)))))

(defmethod params-value-changed ((module-vst3 module-vst3))
  (loop with controller = (.controller module-vst3)
        for param in (.params-ordered module-vst3)
        for value = (vst3-ffi::get-param-normalized controller (.id param))
        do (setf (.value param) value)))

(defun note-buffer->vst3 (module-vst3)
  (loop with note-buffer = (.input-events *process-data*)
        with process-data = (.process-data module-vst3)
        for event across (.events note-buffer)
        for note across (.notes note-buffer)
        for sample-offset across (.sample-offsets note-buffer)
        do (case event
             (:on
              (autowrap:with-alloc (event '(:struct (sb:vst-event)))
                (setf (sb:vst-event.bus-index event) 0) ;TODO
                (setf (sb:vst-event.sample-offset event) sample-offset)
                (setf (sb:vst-event.ppq-position event) .0d0) ;TODO
                (setf (sb:vst-event.flags event) sb:+vst-event-event-flags-k-is-live+) ;TODO
                (setf (sb:vst-event.type event) sb:+vst-event-event-types-k-note-on-event+)
                (setf (sb:vst-event.vst-event-note-on.channel event) (.channel note))
                (setf (sb:vst-event.vst-event-note-on.pitch event) (.key note))
                (setf (sb:vst-event.vst-event-note-on.tuning event) .0)
                (setf (sb:vst-event.vst-event-note-on.velocity event) (.velocity note))
                (setf (sb:vst-event.vst-event-note-on.note-id event) -1)
                (setf (sb:vst-event.vst-event-note-on.length event) 0)
                (vst3-impl::add-event (.input-events process-data)
                                      (autowrap:ptr event)))
              (pushnew (cons (.key note) (.channel note))
                       (.notes-on process-data) :test #'equal))
             (:off
              (autowrap:with-alloc (event '(:struct (sb:vst-event)))
                (setf (sb:vst-event.bus-index event) 0) ;TODO
                (setf (sb:vst-event.sample-offset event) sample-offset)
                (setf (sb:vst-event.ppq-position event) .0d0) ;TODO
                (setf (sb:vst-event.flags event) sb:+vst-event-event-flags-k-is-live+) ;TODO
                (setf (sb:vst-event.type event) sb:+vst-event-event-types-k-note-off-event+)
                (setf (sb:vst-event.vst-event-note-off.channel event) (.channel note))
                (setf (sb:vst-event.vst-event-note-off.pitch event) (.key note))
                (setf (sb:vst-event.vst-event-note-off.velocity event) 1.0)
                (setf (sb:vst-event.vst-event-note-off.note-id event) -1)
                (setf (sb:vst-event.vst-event-note-off.tuning event) .0)
                (vst3-impl::add-event (.input-events process-data) (autowrap:ptr event)))
              (setf (.notes-on module-vst3)
                    (delete (cons (.key note) (.channel note))
                            (.notes-on process-data)
                            :test #'equal))))))

(defmethod process ((self module-vst3))
  (let* ((process-data (.process-data self))
         (context (.context process-data))
         (bpm (.bpm (.project self)))
         (play-time (.play-start (.project self))))
    (setf (sb:vst-process-context.state context)
          (logand (if (.play-p (.project self)) sb:+vst-process-context-states-and-flags-k-playing+ 0)
                  sb:+vst-process-context-states-and-flags-k-tempo-valid+
                  sb:+vst-process-context-states-and-flags-k-project-time-music-valid+
                  sb:+vst-process-context-states-and-flags-k-bar-position-valid+))
    (setf (sb:vst-process-context.tempo context) (coerce bpm 'double-float))
    (setf (sb:vst-process-context.project-time-music context) play-time)
    (setf (sb:vst-process-context.bar-position-music context)
          (coerce (floor (/ play-time 4)) 'double-float))
    (setf (sb:vst-process-context.project-time-samples context)
          ;; TODO これあってる？
          (floor (* (/ play-time (/ bpm 60.0)) (.sample-rate *config*))))

    (setup-audio-buffer (.inputs process-data)
                        (.inputs *process-data*))

    (setf (sb:vst-process-data.input-parameter-changes (.wrap process-data))
          (vst3-impl::ptr (.parameter-changes-out self)))
    (setf (sb:vst-process-data.output-parameter-changes (.wrap process-data))
          (vst3-impl::ptr (.parameter-changes-in self)))

    (prepare (.parameter-changes-in self))
    (prepare (.parameter-changes-out self))

    (loop for message = (sb-concurrency:receive-message-no-hang
                         (.param-changes-mbox-in self))
          with changes = (.parameter-changes-out self)
          while message
          do (destructuring-bind (id value sample-offset) message
               (let ((queue (or (loop for i below (vst3-impl::get-parameter-count changes)
                                      for queue = (vst3-impl::get-parameter-data-wrap changes i)
                                        thereis (and (= (vst3-impl::get-parameter-id queue)
                                                        id)
                                                     queue))
                                (cffi:with-foreign-object (id-ptr :uint)
                                  (setf (cffi:mem-ref id-ptr :uint) id)
                                  (vst3-impl::add-parameter-data-wrap
                                   changes id-ptr (cffi:null-pointer))))))
                 (vst3-impl::add-point queue sample-offset value (cffi:null-pointer)))))

    (vst3-ffi::process (.audio-processor self)
                       (autowrap:ptr (.wrap process-data)))

    (loop with changes = (.parameter-changes-out self)
          for queue-index below (vst3-impl::get-parameter-count changes)
          for queue = (vst3-impl::get-parameter-data-wrap changes queue-index)
          for param-id = (vst3-impl::get-parameter-id queue)
          do (loop for point-index below (vst3-impl::get-point-count queue)
                   do (multiple-value-bind (value sample-offset)
                          (vst3-impl::get-value-and-smaple-offset queue point-index)
                        (sb-concurrency:send-message (.param-changes-mbox-out self)
                                                     (list param-id
                                                           value
                                                           sample-offset)))))

    (call-next-method)))

(defmethod restart-component ((self module-vst3) flags)
  (declare (ignorable flags))
  (log:trace self flags)
  (stop self)
  (start self)
  (when (plusp (logand flags sb:+vst-restart-flags-k-param-values-changed+))
    (params-value-changed self))
  (when (plusp (logand flags sb:+vst-restart-flags-k-param-titles-changed+))
    (params-prepare self))
  ;; TODO 他にも flags に応じていろいろしなきゃいけない
  )

(defmethod start ((self module-vst3))
  (unless (.start-p self)
    (vst3-ffi::set-active (.component self) 1)

    (let ((latency (vst3-ffi::get-latency-samples (.audio-processor self))))
      (when t ;; (/= (.latency self) latency)
        (setf (.latency self) latency)
        (cmd-add (.project self) 'cmd-latency-compute)))

    (vst3-ffi::set-processing (.audio-processor self) 1)
    (call-next-method)))

(defmethod state ((self module-vst3))
  (let ((preset (make-instance 'preset-vst3)))
    (preset-save preset self)
    (preset-vst3-to-base64 preset)))

(defmethod (setf state) (state (self module-vst3))
  (let* ((preset (preset-vst3-from-base64 state)))
    (unless (.component self)
      (load-by-id self (cid preset)))
    (preset-load preset self)))

(defmethod stop ((self module-vst3))
  (when (.start-p self)
    (vst3-ffi::set-processing (.audio-processor self) 0)
    (vst3-ffi::set-active (.component self) 0)
    (call-next-method)))

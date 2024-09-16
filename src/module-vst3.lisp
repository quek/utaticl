(in-package :utaticl.core)

(sb-ext:defglobal *hwnd-module-vst3-map* (make-hash-table :weakness :value))

(defmethod initialize-instance :after ((self module-vst3) &key)
  (let ((host-applicaiton (make-instance 'vst3-impl::host-application :module self)))
    (setf (slot-value self 'host-applicaiton) host-applicaiton)))

(defun module-vst3-load (path)
  (multiple-value-bind (factory library) (vst3::get-plugin-factory path)
    (multiple-value-bind (component id) (vst3::create-component factory)
      (let* ((single-component-p t)
             (controller (labels ((f ()
                                    (setf single-component-p nil)
                                    (vst3::create-instance factory
                                                           (vst3::get-controller-class-id component)
                                                           vst3-ffi::+vst-iedit-controller-iid+)))
                           (handler-case (vst3::query-interface component vst3-ffi::+vst-iedit-controller-iid+)
                             (vst3::no-interface-error () (f))
                             (vst3::false-error () (f))))))
        (make-instance 'module-vst3
                       :id id
                       :library library
                       :factory factory
                       :conponent component
                       :controller controller
                       :single-component-p single-component-p)))))

(defmethod initialize ((self module-vst3))
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
                                              type direction i 1))))
  ;; prepareParameterInfo();
  )

(defmethod terminate ((self module-vst3))
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

  (vst3-ffi::release (.factory self))
  (vst3-impl::release (.host-applicaiton self))
  (vst3::unload-library (.library self)))

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
  (declare (ignore id value-normalized)))

(defmethod end-edit ((self module-vst3) id)
  (declare (ignore id)))

(defmethod restart-component ((self module-vst3) flags)
  (declare (ignorable flags))
  (log:trace self flags)
  (stop self)
  (start self)
  ;; TODO flags に応じた処理
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

(defmethod stop ((self module-vst3))
  (when (.start-p self)
    (vst3-ffi::set-processing (.audio-processor self) 0)
    (vst3-ffi::set-active (.component self) 0)
    (call-next-method)))

(defmethod editor-open ((self module-vst3))
  (unless (.editor-open-p self)
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
          (setf (gethash (cffi:pointer-address hwnd) *hwnd-module-vst3-map*) self)
          (vst3::ensure-ok (vst3-ffi::attached view hwnd vst3-ffi::+k-platform-type-hwnd+)))))
    (call-next-method)))

(defmethod editor-close ((self module-vst3))
  (when (.editor-open-p self)
    (when (.view self)
      (vst3-ffi::set-frame (.view self) (cffi:null-pointer))
      (vst3-ffi::removed (.view self))
      (vst3-ffi::release (.view self))
      (setf (.view self) nil)
      (remhash (cffi:pointer-address (.hwnd self)) *hwnd-module-vst3-map*)
      (ftw:destroy-window (.hwnd self))
      (setf (.hwnd self) nil))
    (call-next-method)))

(defmethod load-by-id ((self module-vst3) id)
  (let* ((plugin-info (plugin-info-find id))
         (path (.path plugin-info)))
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
        (setf (.single-component-p self) single-component-p)))))

(defmethod on-resize ((self module-vst3) width height)
  (when (.editor-open-p self)
    (let ((view (.view self)))
      (autowrap:with-calloc (rect '(:struct (sb:view-rect)))
        (setf (sb:view-rect.right rect) width)
        (setf (sb:view-rect.bottom rect) height)
        (vst3-ffi::on-size view (autowrap:ptr rect))))))

(defmethod process ((self module-vst3))
  (let ((context (.context *process-data*))
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
          (floor (* (/ play-time (/ bpm 60.0)) (.sample-rate *config*)))))

  (vst3-ffi::process (.audio-processor self)
                     (autowrap:ptr (.wrap *process-data*)))

  (call-next-method))

(defmethod state ((self module-vst3))
  (let ((preset (make-instance 'preset-vst3)))
    (preset-save preset self)
    (preset-vst3-to-base64 preset)))

(defmethod (setf state) (state (self module-vst3))
  (let* ((preset (preset-vst3-from-base64 state)))
    (unless (.component self)
      (load-by-id self (cid preset))
      (initialize self))
    (preset-load preset self)))


#+nil
(sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
 (let ((module (module-vst3-load
                "c:/Program Files/Common Files/VST3/Dexed.vst3"
                ;;"c:/Program Files/Common Files/VST3/DS Thorn.vst3"
                ;;"c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3"
                ;;"c:/Program Files/Common Files/VST3/Vital.vst3"
                )))
   (initialize module)
   (start module)
   (editor-open module)
   (sleep 5)
   (editor-close module)
   (stop module)
   (terminate module)
   module))

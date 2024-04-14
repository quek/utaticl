(in-package :dgw)

(defclass vst3-module (module)
  ((host-applicaiton :reader .host-applicaiton)
   (factory :initarg :factory :reader .factory)
   (component :initarg :conponent :reader .component)
   (controller :initarg :controller :reader .controller)
   (single-component-p :initarg :single-component-p :reader .single-component-p)
   (process :accessor .process)
   (audio-input-bus-count :accessor .audio-input-bus-count)
   (audio-output-bus-count :accessor .audio-output-bus-count)
   (event-input-bus-count :accessor .event-input-bus-count)
   (event-output-bus-count :accessor .event-output-bus-count)
   (view :initform :nil :accessor .view)
   (hwnd :initform :nil :accessor .hwnd)
   (process-data :reader .process-data)
   (buffer-in :initform (list (make-array 1024 :element-type 'single-float :initial-element 0.0)
                              (make-array 1024 :element-type 'single-float :initial-element 0.0))
              :accessor .buffer-in)
   (buffer-out :initform (list (make-array 1024 :element-type 'single-float :initial-element 0.0)
                               (make-array 1024 :element-type 'single-float :initial-element 0.0))
               :accessor .buffer-out)
   (parameter-changes-in :initform (make-instance 'vst3-impl::parameter-changes)
                         :accessor .parameter-changes-in)))

(defmethod initialize-instance :after ((self vst3-module) &key)
  (let ((host-applicaiton (make-instance 'vst3-impl::host-application :module self))
        (process-data (autowrap:alloc '(:struct (vst3-c-api:steinberg-vst-process-data))))
        (inputs (autowrap:alloc '(:struct (vst3-c-api:steinberg-vst-audio-bus-buffers))))
        (outputs (autowrap:alloc '(:struct (vst3-c-api:steinberg-vst-audio-bus-buffers)))))
    (vst3-impl::add-ref host-applicaiton)
    (setf (slot-value self 'host-applicaiton) host-applicaiton)

    (setf (slot-value self 'process-data) process-data)
    (setf (vst3-c-api:steinberg-vst-process-data.process-mode process-data)
          vst3-c-api:+steinberg-vst-process-modes-k-realtime+)
    (setf (vst3-c-api::steinberg-vst-process-data.symbolic-sample-size process-data)
          vst3-c-api::+steinberg-vst-symbolic-sample-sizes-k-sample32+)
    (setf (vst3-c-api:steinberg-vst-process-data.num-samples process-data)
          *frames-per-buffer*)
    (setf (vst3-c-api:steinberg-vst-process-data.num-inputs process-data)
          1)
    (setf (vst3-c-api:steinberg-vst-process-data.num-outputs process-data)
          1)
    (setf (vst3-c-api:steinberg-vst-process-data.inputs process-data)
          (autowrap:ptr inputs))
    (setf (vst3-c-api:steinberg-vst-process-data.outputs process-data)
          (autowrap:ptr outputs))
    (setf (vst3-c-api:steinberg-vst-process-data.input-parameter-changes process-data)
          (vst3-impl::ptr (.parameter-changes-in self)))
    (setf (vst3-c-api:steinberg-vst-process-data.output-parameter-changes process-data)
          (cffi:null-pointer))
    (setf (vst3-c-api:steinberg-vst-process-data.input-events process-data)
          (cffi:null-pointer))
    (setf (vst3-c-api:steinberg-vst-process-data.output-events process-data)
          (cffi:null-pointer))
    (setf (vst3-c-api:steinberg-vst-process-data.process-context process-data)
          (cffi:null-pointer))

    (setf (vst3-c-api::steinberg-vst-audio-bus-buffers.num-channels inputs) 2)
    (setf (vst3-c-api::steinberg-vst-audio-bus-buffers.silence-flags inputs) 0)
    (let ((channels (autowrap:alloc :pointer 2)))
      (loop for i below 2
            do (setf (cffi:mem-ref channels :pointer i)
                     (sb-sys:vector-sap (nth i (.buffer-in self)))))
      (setf (vst3-c-api::steinberg-vst-audio-bus-buffers.steinberg-vst-audio-bus-buffers-channel-buffers32 inputs)
            channels))
    
    (setf (vst3-c-api::steinberg-vst-audio-bus-buffers.num-channels outputs) 2)
    (setf (vst3-c-api::steinberg-vst-audio-bus-buffers.silence-flags outputs) 0)
    (let ((channels (autowrap:alloc :pointer 2)))
      (loop for i below 2
            do (setf (cffi:mem-ref channels :pointer i)
                     (sb-sys:vector-sap (nth i (.buffer-out self)))))
            (setf (vst3-c-api::steinberg-vst-audio-bus-buffers.steinberg-vst-audio-bus-buffers-channel-buffers32 outputs)
            channels))))

(defun vst3-module-load (path)
  (let* ((factory (vst3::get-plugin-factory path))
         (component (vst3::create-component factory))
         (single-component-p t)
         (controller (handler-case (vst3::query-interface component vst3-ffi::+steinberg-vst-iedit-controller-iid+)
                       (vst3::no-interface-error ()
                         (setf single-component-p nil)
                         (vst3::create-instance factory
                                                (vst3::get-controller-class-id component)
                                                vst3-ffi::+steinberg-vst-iedit-controller-iid+)))))
    (make-instance 'vst3-module
                   :factory factory
                   :conponent component
                   :controller controller
                   :single-component-p single-component-p)))

(defmethod initialize ((self vst3-module))
  (vst3-ffi::initialize (.component self)
                        (vst3-impl::ptr (.host-applicaiton self)))
  (vst3-ffi::initialize (.controller self)
                        (vst3-impl::ptr (.host-applicaiton self)))
  
  (connect-componet-controller self)
  
  (let ((bstream (make-instance 'vst3-impl::bstream)))
    (vst3::ensure-ok (vst3-ffi::get-state (.component self) (vst3-impl::ptr bstream)))
    (setf (vst3-impl::.cursor bstream) 0)
    (handler-case
        (vst3::ensure-ok (vst3-ffi::set-component-state (.controller self) (vst3-impl::ptr bstream)))
      (vst3::not-implemented-error ()
        ;; 無視してだいじょうぶなやつ
        )))

  (vst3-ffi::set-component-handler
   (.controller self)
   (vst3-impl::ptr (vst3-impl::.component-handler (.host-applicaiton self))))
  
  (let ((process (vst3::query-interface
                  (.component self) vst3-ffi::+steinberg-vst-iaudio-processor-iid+))
        (audio-input-bus-count (vst3-ffi::get-bus-count
                                (.component self) vst3-c-api::+steinberg-vst-media-types-k-audio+
                                vst3-c-api::+steinberg-vst-bus-directions-k-input+))
        (audio-output-bus-count (vst3-ffi::get-bus-count
                                 (.component self) vst3-c-api::+steinberg-vst-media-types-k-audio+
                                 vst3-c-api::+steinberg-vst-bus-directions-k-output+))
        (event-input-bus-count (vst3-ffi::get-bus-count
                                (.component self) vst3-c-api::+steinberg-vst-media-types-k-event+
                                vst3-c-api::+steinberg-vst-bus-directions-k-input+))
        (event-output-bus-count (vst3-ffi::get-bus-count
                                 (.component self) vst3-c-api::+steinberg-vst-media-types-k-event+
                                 vst3-c-api::+steinberg-vst-bus-directions-k-output+)))
    (setf (.process self) process)
    (setf (.audio-input-bus-count self) audio-input-bus-count)
    (setf (.audio-output-bus-count self) audio-output-bus-count)
    (setf (.event-input-bus-count self) event-input-bus-count)
    (setf (.event-output-bus-count self) event-output-bus-count)

    (vst3::ensure-ok
     (vst3-ffi::can-process-sample-size process vst3-c-api::+steinberg-vst-symbolic-sample-sizes-k-sample32+))

    (vst3-ffi::set-processing (.process self) 0)
    (vst3-ffi::set-active (.component self) 0)

    (autowrap:with-alloc (setup '(:struct (vst3-c-api::steinberg-vst-process-setup)))
      (setf (vst3-c-api::steinberg-vst-process-setup.process-mode setup)
            vst3-c-api::+steinberg-vst-process-modes-k-realtime+)
      (setf (vst3-c-api::steinberg-vst-process-setup.symbolic-sample-size setup)
            vst3-c-api::+steinberg-vst-symbolic-sample-sizes-k-sample32+)
      (setf (vst3-c-api::steinberg-vst-process-setup.max-samples-per-block  setup)
            1024)
      (setf (vst3-c-api::steinberg-vst-process-setup.sample-rate setup)
            48000.0d0)
      (vst3::ensure-ok (vst3-ffi::setup-processing process (autowrap:ptr setup))))

    (loop for (count type direction) in `((,audio-input-bus-count
                                           ,vst3-c-api::+steinberg-vst-media-types-k-audio+
                                           ,vst3-c-api::+steinberg-vst-bus-directions-k-input+)
                                          (,audio-output-bus-count
                                           ,vst3-c-api::+steinberg-vst-media-types-k-audio+
                                           ,vst3-c-api::+steinberg-vst-bus-directions-k-output+)
                                          (,event-input-bus-count
                                           ,vst3-c-api::+steinberg-vst-media-types-k-event+
                                           ,vst3-c-api::+steinberg-vst-bus-directions-k-input+)
                                          (,event-output-bus-count
                                           ,vst3-c-api::+steinberg-vst-media-types-k-event+
                                           ,vst3-c-api::+steinberg-vst-bus-directions-k-output+))
          do (loop for i below count
                   do (vst3-ffi::activate-bus (.component self)
                                              type direction i 1))))

  ;; prepareParameterInfo();
  )

(defmethod terminate ((self vst3-module))
  (disconnect-componet-controller self)
  (vst3-ffi::terminate (.component self))
  (unless (.single-component-p self)
    (vst3-ffi::terminate (.controller self)))
  (vst3-impl::release (.host-applicaiton self))
  (autowrap:free (.process-data self)))

(defmethod connect-componet-controller ((self vst3-module))
  (unless (.single-component-p self)
    (handler-case
        (let ((c1 (vst3::query-interface (.component self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+))
              (c2 (vst3::query-interface (.controller self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+)))
          (vst3-ffi::connect c1 (vst3-walk::.ptr c2))
          (vst3-ffi::connect c2 (vst3-walk::.ptr c1)))
      (vst3::no-interface-error ()))))

(defmethod disconnect-componet-controller ((self vst3-module))
  (unless (.single-component-p self)
    (handler-case
        (let ((c1 (vst3::query-interface (.component self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+))
              (c2 (vst3::query-interface (.controller self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+)))
          (vst3-ffi::disconnect c1 (vst3-walk::.ptr c2))
          (vst3-ffi::disconnect c2 (vst3-walk::.ptr c1)))
      (vst3::no-interface-error ()))))

(defmethod begin-edit ((self vst3-module) id)
  (declare (ignore id)))

(defmethod perform-edit ((self vst3-module) id value-normalized)
  (declare (ignore id value-normalized)))

(defmethod end-edit ((self vst3-module) id)
  (declare (ignore id)))

(defmethod restart-component ((self vst3-module) flags)
  (declare (ignore flags)))

(defmethod start ((self vst3-module))
  (unless (.start-p self)
    (vst3-ffi::set-active (.component self) 1)
    (vst3-ffi::set-processing (.process self) 1)
    (call-next-method)))

(defmethod stop ((self vst3-module))
  (when (.start-p self)
    (vst3-ffi::set-processing (.process self) 0)
    (vst3-ffi::set-active (.component self) 0)
    (call-next-method)))

(defmethod editor-open ((self vst3-module))
  (unless (.editor-open-p self)
    (let* ((view-ptr (vst3-ffi::create-view (.controller self)
                                            vst3-ffi::+steinberg-vst-view-type-k-editor+))
           (view (make-instance 'vst3-ffi::steinberg-iplug-view  :ptr view-ptr)))
      (setf (.view self) view)
      (vst3-ffi::set-frame view (vst3-impl::ptr (vst3-impl::.plug-frame
                                                 (.host-applicaiton self))))
      (autowrap:with-alloc (size '(:struct (vst3-c-api:steinberg-view-rect)))
        (vst3-ffi::get-size view (autowrap:ptr size))
        (let* ((resizable (= (vst3-ffi::can-resize view) vst3-c-api:+steinberg-k-result-true+))
               (width (- (vst3-c-api:steinberg-view-rect.right size)
                         (vst3-c-api:steinberg-view-rect.left size)))
               (height (- (vst3-c-api:steinberg-view-rect.bottom size)
                          (vst3-c-api:steinberg-view-rect.top size)))
               (hwnd (win32::make-window width height resizable)))
          (setf (.hwnd self) hwnd)
          (vst3::ensure-ok (vst3-ffi::attached view hwnd vst3-ffi::+steinberg-k-platform-type-hwnd+)))))
    (call-next-method)))

(defmethod editor-close ((self vst3-module))
  (when (.editor-open-p self)
    (when (.view self)
      (vst3-ffi::removed (.view self))
      (vst3-ffi::release (.view self))
      (sb-ext:cancel-finalization (.view self))
      (setf (.view self) nil)
      (ftw:destroy-window (.hwnd self))
      (setf (.hwnd self) nil))
    (call-next-method)))

(defmethod process ((self module))
  (let ((process-data (.process-data self)))
    (vst3-ffi::process (.process self)
                       (autowrap:ptr process-data))))



#+nil
(let ((module (vst3-module-load
               "c:/Program Files/Common Files/VST3/Dexed.vst3"
               ;;"c:/Program Files/Common Files/VST3/DS Thorn.vst3"
               )))
  (initialize module)
  (start module)
  (editor-open module)

  (editor-close module)
  (stop module)
  (terminate module)
  module)

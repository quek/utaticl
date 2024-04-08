(in-package :dgw)

(defclass module-vst3 (module)
  ((host-applicaiton
    :initform (make-instance 'vst3-host-application::host-application)
    :reader .host-applicaiton)
   (factory :initarg :factory :reader .factory)
   (component :initarg :conponent :reader .component)
   (controller :initarg :controller :reader .controller)
   (single-component-p :initarg :single-component-p :reader .single-component-p)))

(defun module-vst3-load (path)
  (let* ((factory (vst3::get-plugin-factory path))
         (component (vst3::create-component factory))
         (single-component-p t)
         (controller (handler-case (vst3::query-interface component vst3-ffi::+steinberg-vst-iedit-controller-iid+)
                       (vst3::no-interface-error ()
                         (setf single-component-p nil)
                         (vst3::create-instance factory
                                                (vst3::get-controller-class-id component)
                                                vst3-ffi::+steinberg-vst-iedit-controller-iid+)))))
    (make-instance 'module-vst3
                   :factory factory
                   :conponent component
                   :controller controller
                   :single-component-p single-component-p)))

(defmethod initialize ((self module-vst3))
  (vst3-ffi::initialize (.component self)
                        (autowrap:ptr (vst3-host-application::.wrap (.host-applicaiton self))))
  (vst3-ffi::initialize (.controller self)
                        (autowrap:ptr (vst3-host-application::.wrap (.host-applicaiton self))))
  (connect-componet-controller self)
  (let ((bstream ))))

(defmethod terminate ((self module-vst3))
  (disconnect-componet-controller self)
  (vst3-ffi::terminate (.component self))
  (unless (.single-component-p self)
    (vst3-ffi::terminate (.controller self))))

(defmethod connect-componet-controller ((self module-vst3))
  (unless (.single-component-p self)
    (handler-case
        (let ((c1 (vst3::query-interface (.component self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+))
              (c2 (vst3::query-interface (.controller self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+)))
          (vst3-ffi::connect c1 (vst3-walk::.ptr c2))
          (vst3-ffi::connect c2 (vst3-walk::.ptr c1)))
      (vst3::no-interface-error ()))))

(defmethod disconnect-componet-controller ((self module-vst3))
  (unless (.single-component-p self)
    (handler-case
        (let ((c1 (vst3::query-interface (.component self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+))
              (c2 (vst3::query-interface (.controller self)
                                         vst3-ffi::+steinberg-vst-iconnection-point-iid+)))
          (vst3-ffi::disconnect c1 (vst3-walk::.ptr c2))
          (vst3-ffi::disconnect c2 (vst3-walk::.ptr c1)))
      (vst3::no-interface-error ()))))

#+nil
(let ((module (module-vst3-load "c:/Program Files/Common Files/VST3/Dexed.vst3")))
  (initialize module)
  (terminate module))

(in-package :dgw)

(defclass module-vst3 (module)
  ((factory :initarg :factory :reader .factory)
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


#+nil
(module-vst3-load "c:/Program Files/Common Files/VST3/Dexed.vst3")
;;⇒ #<MODULE-VST3 {1003BD1A83}>


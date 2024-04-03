(in-package :dgw)

(defclass module-vst3 (module)
  ((library :accessor .library)))

(defun module-vst3-load (path)
  (let* ((factory (vst3::get-plugin-factory path))
         (factory2 (vst3::query-interface factory vst3::*iplugin-factory2-iid*))
         (factory3 (vst3::query-interface factory vst3::*iplugin-factory3-iid*))
         (class-info
           (loop for i below (vst3::count-classes factory)
                 for class-info = (or (and factory3 (vst3::get-class-info-unicode factory3 i))
                                      (and factory2 (vst3::get-class-info2 factory2 i))
                                      (vst3::get-class-info factory i))
                   thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                                class-info))))
    (let ((component (vst3::create-instance factory (vst3::.cid class-info) vst3::*icomponent-iid*)))
      component)))

#+nil
(module-vst3-load "c:/Program Files/Common Files/VST3/Dexed.vst3")
;;⇒ #<VST3::COMPONENT {10039E5B23}>



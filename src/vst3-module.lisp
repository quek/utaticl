(in-package :dgw)

(defclass module-vst3 (module)
  ((library :accessor .library)))

(defvar *x* nil)


(defun module-vst3-load (path)
  (let* ((factory (vst3::get-plugin-factory path))
         (factory (or (vst3::query-interface factory vst3::*iplugin-factory3-iid*)
                      (vst3::query-interface factory vst3::*iplugin-factory2-iid*))))
    (let ((component (vst3::create-component factory)))
      component)))


#+nil
(module-vst3-load "c:/Program Files/Common Files/VST3/Dexed.vst3")
;;⇒ #<VST3::VST-I-COMPONENT {1001028823}>






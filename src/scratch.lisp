(in-package :dgw)

;;(cffi:defctype tuid :array :char 16)


(setf dexed (cffi:load-foreign-library "c:/Program Files/Common Files/VST3/Dexed.vst3"))
;;⇒ #<CFFI:FOREIGN-LIBRARY DEXED.VST3-773 "Dexed.vst3">

(cffi:foreign-funcall-pointer
 (cffi:foreign-symbol-pointer "InitDll" :library dexed) ())
;;⇒ 

(setf dexed-plugin-factory (cffi:foreign-funcall-pointer
  (cffi:foreign-symbol-pointer "GetPluginFactory" :library dexed) ()
  :pointer))
;;⇒ #.(SB-SYS:INT-SAP #X006BC650)

(let ((vtbl (cffi:foreign-slot-value dexed-plugin-factory '(:struct vst3::iplugin-factory)
                                     'vst3::vtbl)))
 (cffi::foreign-funcall-pointer
  (cffi::foreign-slot-value vtbl '(:struct vst3::iplugin-factory-vtbl)
                            'vst3::count-classes)
  () :pointer dexed-plugin-factory
  :int32))
;;⇒ 2

;; grovel の cstruct-and-class-item で作られたクラスを使ってみるも、全然便利じゃないね
(let* ((plugin-factory (vst3::make-iplugin-factory-from-pointer dexed-plugin-factory))
       (plugin-factory-vtbl (vst3::make-iplugin-factory-vtbl-from-pointer
                             (vst3::iplugin-factory-vtbl plugin-factory))))
  (cffi:foreign-funcall-pointer
   (vst3::iplugin-factory-vtbl-count-classes plugin-factory-vtbl) ()
   :pointer dexed-plugin-factory
   :int32))
;;⇒ 2

;; これなら使えるかな。macro で作りこまなきゃだけど
(let ((x (make-instance 'vst3::plugin-factory :ptr dexed-plugin-factory)))
  (vst3::count-classes x))
;;⇒ 2

(let ((x (make-instance 'vst3::plugin-factory :ptr dexed-plugin-factory)))
  (vst3::query-interface x vst3::*iplugin-factory-iid*))
;;⇒ #<VST3::PLUGIN-FACTORY {1003C32543}>

(let ((x (make-instance 'vst3::unknown :ptr dexed-plugin-factory)))
  (vst3::query-interface x vst3::*iplugin-factory-iid*))
;;⇒ #<VST3::PLUGIN-FACTORY {1003CDE883}>

(let* ((x (make-instance 'vst3::plugin-factory :ptr dexed-plugin-factory))
       (f3 (vst3::query-interface x vst3::*iplugin-factory3-iid*)))
  (vst3::get-class-info-unicode f3 0))



(in-package :dgw)

;;(cffi:defctype tuid :array :char 16)


(defvar *dexed* (cffi:load-foreign-library "c:/Program Files/Common Files/VST3/Dexed.vst3"))
;;⇒ *DEXED*

(cffi:foreign-funcall-pointer
 (cffi:foreign-symbol-pointer "InitDll" :library *dexed*) ())
;;⇒ 

(defvar *dexed-plugin-factory* (cffi:foreign-funcall-pointer
  (cffi:foreign-symbol-pointer "GetPluginFactory" :library *dexed*) ()
  :pointer))
;;⇒ *DEXED-PLUGIN-FACTORY*

(let ((vtbl (cffi:foreign-slot-value *dexed-plugin-factory* '(:struct vst3::iplugin-factory)
                                     'vst3::vtbl)))
 (cffi::foreign-funcall-pointer
  (cffi::foreign-slot-value vtbl '(:struct vst3::iplugin-factory-vtbl)
                            'vst3::count-classes)
  () :pointer *dexed-plugin-factory*
  :int32))
;;⇒ 2

;; grovel の cstruct-and-class-item で作られたクラスを使ってみるも、全然便利じゃないね
(let* ((plugin-factory (vst3::make-iplugin-factory-from-pointer *dexed-plugin-factory*))
       (plugin-factory-vtbl (vst3::make-iplugin-factory-vtbl-from-pointer
                             (vst3::iplugin-factory-vtbl plugin-factory))))
  (cffi:foreign-funcall-pointer
   (vst3::iplugin-factory-vtbl-count-classes plugin-factory-vtbl) ()
   :pointer *dexed-plugin-factory*
   :int32))
;;⇒ 2

;; これなら使えるかな。macro で作りこまなきゃだけど
(let ((x (make-instance 'vst3::plugin-factory :ptr *dexed-plugin-factory*)))
  (vst3::count-classes x))
;;⇒ 2

(let* ((x (make-instance 'vst3::plugin-factory :ptr *dexed-plugin-factory*))
       (f2 (vst3::query-interface x vst3::*iplugin-factory2-iid*)))
  (vst3::count-classes f2))
;;⇒ 2

(let ((x (make-instance 'vst3::unknown :ptr *dexed-plugin-factory*)))
  (vst3::query-interface x vst3::*iplugin-factory3-iid*))
;;⇒ #<VST3::PLUGIN-FACTORY3 {1006926C33}>

(let* ((x (make-instance 'vst3::plugin-factory :ptr *dexed-plugin-factory*))
       (factory3 (vst3::query-interface x vst3::*iplugin-factory3-iid*)))
  (loop for i below (vst3::count-classes factory3)
        collect (vst3::get-class-info-unicode factory3 0)))
;;⇒ (#<VST3::PCLASS-INFO-W {100743DE83}> #<VST3::PCLASS-INFO-W {100743E2A3}>)






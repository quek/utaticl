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

(let ((vtbl (cffi:foreign-slot-value dexed-plugin-factory '(:struct vst3::iplugin-factory)
                                     'vst3::vtbl)))
 (cffi::foreign-funcall-pointer
  (cffi::foreign-slot-value vtbl '(:struct vst3::iplugin-factory-vtbl)
                            'vst3::count-classes)
  () :pointer dexed-plugin-factory
  :int32))
;;⇒ 2

(let ((vtbl (cffi:foreign-slot-value dexed-plugin-factory '(:struct vst3::funknown)
                                     'vst3::vtbl)))
 (cffi:with-foreign-objects ((obj '(:pointer :void)))
   (cffi::foreign-funcall-pointer
    (cffi::foreign-slot-value vtbl '(:struct vst3::funknown-vtbl)
                              'vst3::query-interface)
    () :pointer dexed-plugin-factory
    :pointer (sb-sys:vector-sap *funknown-iid*)
    :pointer obj
    vst3::tresult)))




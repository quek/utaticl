(in-package :dgw)

(let* ((x (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3")))
  (vst3-ffi::count-classes x))

(let* ((factory (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3"))
       (factory3 (cffi:with-foreign-object (obj :pointer)
                   (if (= vst3-c-api::+steinberg-k-result-ok+
                          (vst3-ffi::query-interface factory vst3-ffi::+steinberg-iplugin-factory3-iid+ obj))
                       (make-instance 'vst3-ffi::steinberg-iplugin-factory3 :ptr (cffi:mem-ref obj :pointer))))))
  (vst3-ffi::count-classes factory3))






(let* ((x (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3"))
       (factory3 (vst3::query-interface x vst3::*iplugin-factory3-iid*)))
  (loop for i below (vst3::count-classes factory3)
        collect (vst3::get-class-info-unicode factory3 i)))
;;⇒ (#<VST3::CLASS-INFO-W {10052671D3}> #<VST3::CLASS-INFO-W {1005267413}>)

(let* ((x (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3"))
       (factory3 (vst3::query-interface x vst3::*iparameter-changes-iid*)))
  factory3)



(let ((library (cffi:load-foreign-library "c:/Program Files/Common Files/VST3/Dexed.vst3")))
  (cffi:foreign-funcall-pointer
   (cffi:foreign-symbol-pointer "InitDll" :library library) ())
  (let ((factory (vst3-c-api::make-steinberg-i-plugin-factory
                  :ptr (cffi:foreign-funcall-pointer
                        (cffi:foreign-symbol-pointer "GetPluginFactory" :library library) ()
                        :pointer))))
    (let ((factory3 (cffi:with-foreign-objects ((ptr '(:pointer :void)))
                      (if (= vst3-c-api::+steinberg-k-result-ok+
                             (cffi:foreign-funcall-pointer
                              (vst3-c-api::steinberg-i-plugin-factory.lp-vtbl*.query-interface factory)
                              ()
                              :pointer (vst3-c-api::steinberg-i-plugin-factory-ptr factory)
                              :pointer (sb-sys:vector-sap vst3::*iplugin-factory3-iid*)
                              :pointer ptr
                              vst3::tresult))
                          (vst3-c-api::make-steinberg-i-plugin-factory3 :ptr (cffi:mem-ref ptr :pointer))
                          nil))))
      (loop for i below (cffi:foreign-funcall-pointer
                         (vst3-c-api::steinberg-i-plugin-factory-vtbl.count-classes
                          (vst3-c-api::steinberg-i-plugin-factory.lp-vtbl* factory))
                         ()
                         :pointer (vst3-c-api::steinberg-i-plugin-factory-ptr factory)
                         :uint32)
            collect (cffi:with-foreign-objects ((class-info :pointer))
                      (if (= vst3-c-api::+steinberg-k-result-ok+
                             (cffi:foreign-funcall-pointer
                              (vst3-c-api:steinberg-i-plugin-factory3.lp-vtbl*.get-class-info-unicode factory3)
                              ()
                              :pointer (vst3-c-api::steinberg-i-plugin-factory3-ptr factory3)
                              :int32 i
                              :pointer class-info
                              vst3::tresult))
                          (vst3-c-api::make-steinberg-p-class-info-w :ptr (cffi:mem-ref class-info :pointer))
                          nil))))))
;;⇒ (#<VST3-C-API:STEINBERG-P-CLASS-INFO-W {#XFAEB9182ABCDEF01}>
;;    #<VST3-C-API:STEINBERG-P-CLASS-INFO-W {#XABCD1234ABCDEF01}>)



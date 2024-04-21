(in-package :dgw)

(let* ((factory (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3")))
  (vst3::create-component factory))
;;⇒ #<VST3-FFI::STEINBERG-VST-ICOMPONENT {1004590EB3}>




(let* ((path "c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3")
       ;; (path "c:/Program Files/Common Files/VST3/Dexed.vst3")
       (library (cffi:load-foreign-library path))
       (factory (progn
                  (cffi:foreign-funcall-pointer
                   (cffi:foreign-symbol-pointer "InitDll" :library library) ())
                  (cffi:foreign-funcall-pointer
                   (cffi:foreign-symbol-pointer "GetPluginFactory" :library library) ()
                   :pointer)))
       (factory (vst3-c-api::make-steinberg-i-plugin-factory :ptr factory))
       (factory3 (cffi:with-foreign-object (obj :pointer)
                   (cffi:foreign-funcall-pointer
                    (vst3-c-api:steinberg-i-plugin-factory.lp-vtbl*.query-interface factory)
                    ()
                    :pointer (autowrap:ptr factory)
                    :pointer (sb-sys:vector-sap vst3-ffi::+steinberg-iplugin-factory3-iid+)
                    :pointer obj
                    :int)
                   (vst3-c-api::make-steinberg-i-plugin-factory3 :ptr (cffi:mem-ref obj :pointer))))
       (component (autowrap:with-alloc (%class-info '(:struct (vst3-c-api:steinberg-p-class-info-w)))
                    (loop for index below (cffi:foreign-funcall-pointer
                                           (vst3-c-api:steinberg-i-plugin-factory3.lp-vtbl*.count-classes factory3)
                                           ()
                                           :pointer (autowrap:ptr factory3)
                                           :int)
                          for class-info = (progn
                                             (vst3::ensure-ok
                                              (cffi:foreign-funcall-pointer
                                               (vst3-c-api:steinberg-i-plugin-factory3.lp-vtbl*.get-class-info-unicode factory3)
                                               ()
                                               :pointer (autowrap:ptr factory3)
                                               :int index
                                               :pointer (autowrap:ptr %class-info)
                                               :int))
                                             (vst3-c-api::make-steinberg-p-class-info-w :ptr (autowrap:ptr %class-info)))
                            thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                                         (cffi:with-foreign-object (obj :pointer)
                                           (vst3::ensure-ok
                                            (cffi:foreign-funcall-pointer
                                             (vst3-c-api:steinberg-i-plugin-factory.lp-vtbl*.create-instance factory)
                                             ()
                                             :pointer (autowrap:ptr factory)
                                             :pointer (vst3-c-api:steinberg-p-class-info-w.cid& class-info)
                                             :pointer (sb-sys:vector-sap vst3-ffi::+steinberg-vst-icomponent-iid+)
                                             :pointer obj
                                             :int))
                                           (vst3-c-api::make-steinberg-vst-i-component :ptr (cffi:mem-ref obj :pointer))))))))
  component)

(let* ((path "c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3")
       ;; (path "c:/Program Files/Common Files/VST3/Dexed.vst3")
       (library (cffi:load-foreign-library path))
       (factory (progn
                  (cffi:foreign-funcall-pointer
                   (cffi:foreign-symbol-pointer "InitDll" :library library) ())
                  (cffi:foreign-funcall-pointer
                   (cffi:foreign-symbol-pointer "GetPluginFactory" :library library) ()
                   :pointer)))
       (factory (vst3-c-api::make-steinberg-i-plugin-factory :ptr factory))
       (component (autowrap:with-alloc (%class-info '(:struct (vst3-c-api:steinberg-p-class-info)))
                    (loop for index below (cffi:foreign-funcall-pointer
                                           (vst3-c-api:steinberg-i-plugin-factory.lp-vtbl*.count-classes factory)
                                           ()
                                           :pointer (autowrap:ptr factory)
                                           :int)
                          for class-info = (progn
                                             (vst3::ensure-ok
                                              (cffi:foreign-funcall-pointer
                                               (vst3-c-api:steinberg-i-plugin-factory.lp-vtbl*.get-class-info factory)
                                               ()
                                               :pointer (autowrap:ptr factory)
                                               :int index
                                               :pointer (autowrap:ptr %class-info)
                                               :int))
                                             (vst3-c-api::make-steinberg-p-class-info :ptr (autowrap:ptr %class-info)))
                            thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                                         (cffi:with-foreign-object (obj :pointer)
                                           (vst3::ensure-ok
                                            (cffi:foreign-funcall-pointer
                                             (vst3-c-api:steinberg-i-plugin-factory.lp-vtbl*.create-instance factory)
                                             ()
                                             :pointer (autowrap:ptr factory)
                                             :pointer (vst3-c-api:steinberg-p-class-info.cid& class-info)
                                             :pointer (sb-sys:vector-sap vst3-ffi::+steinberg-vst-icomponent-iid+)
                                             :pointer obj
                                             :int))
                                           (vst3-c-api::make-steinberg-vst-i-component :ptr (cffi:mem-ref obj :pointer))))))))
  component)

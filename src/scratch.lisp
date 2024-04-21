(in-package :dgw)

(let* ((factory (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3")))
  (vst3::create-component factory))
;;⇒ #<VST3-FFI::STEINBERG-VST-ICOMPONENT {1005053A93}>

(let* ((factory (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3")))
  (vst3::create-component factory))

(let* ((path "c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3")
       ;;(path "c:/Program Files/Common Files/VST3/Dexed.vst3")
       ;;p(path "c:/Program Files/Common Files/VST3/F-em (64 bit).vst3")
       ;;(path "c:/Program Files/Common Files/VST3/Kilohearts/Phase Plant.vst3")
       ;;(path "c:/Program Files/Common Files/VST3/DS Thorn.vst3")
       (factory (vst3::get-plugin-factory path))
       (component (autowrap:with-alloc (%class-info '(:struct (vst3-c-api:steinberg-p-class-info)))
                    (loop for index below (vst3-ffi::count-classes factory)
                          for class-info = (progn
                                             (vst3-ffi::get-class-info factory index (autowrap:ptr %class-info))
                                             (vst3-c-api::make-steinberg-p-class-info :ptr (autowrap:ptr %class-info)))
                            thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                                         (cffi:with-foreign-object (obj :pointer)
                                           (vst3-ffi::create-instance factory
                                                                      (vst3-c-api:steinberg-p-class-info.cid& class-info)
                                                                      (sb-sys:vector-sap vst3-ffi::+steinberg-vst-icomponent-iid+)
                                                                      obj)
                                           (vst3-c-api::make-steinberg-vst-i-component :ptr (cffi:mem-ref obj :pointer))))))))
  component)

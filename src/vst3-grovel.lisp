(in-package :vst3)

(include "c:/Users/ancient/quicklisp/local-projects/dgw/lib/vst3_c_api/vst3_c_api.h")

(ctype tresult "Steinberg_tresult")
(cvar ("Steinberg_kResultOk" kresult-ok) tresult :read-only t)

(cstruct-and-class-item pclass-info-w "struct Steinberg_PClassInfoW"
                        (cid "cid" :type :pointer)
                        (cardinality "cardinality" :type :int32)
                        (category "category" :type :char :count 32)
                        (name "name" :type char16 :count 64)
                        (class-flags "classFlags" :type :uint32)
                        (sub-categories "subCategories" :type :char :count 128)
                        (vendor "vendor" :type char16 :count 64)
                        (version "version" :type char16 :count 64)
                        (sdk-version "sdkVersion" :type dgw::char16 :count 64)) 


(cstruct-and-class-item funknown-vtbl "Steinberg_FUnknownVtbl"
                        (query-interface "queryInterface" :type :pointer)
                        (add-ref "addRef" :type :pointer)
                        (release "release" :type :pointer))

(cstruct-and-class-item funknown "Steinberg_FUnknown"
                        (vtbl "lpVtbl" :type (:pointer (:struct funknown-vtbl))))

(cstruct-and-class-item iplugin-factory-vtbl "Steinberg_IPluginFactoryVtbl"
                        (query-interface "queryInterface" :type :pointer)
                        (add-ref "addRef" :type :pointer)
                        (release "release" :type :pointer)
                        (get-factory-info "getFactoryInfo" :type :pointer)
                        (count-classes "countClasses" :type :pointer)
                        (get-class-info "getClassInfo" :type :pointer)
                        (create-instance "createInstance" :type :pointer))

(cstruct-and-class-item iplugin-factory "Steinberg_IPluginFactory"
                        (vtbl "lpVtbl" :type (:pointer (:struct iplugin-factory-vtbl))))

(cstruct-and-class-item iplugin-factory2-vtbl "Steinberg_IPluginFactory2Vtbl"
                        (query-interface "queryInterface" :type :pointer)
                        (add-ref "addRef" :type :pointer)
                        (release "release" :type :pointer)
                        (get-factory-info "getFactoryInfo" :type :pointer)
                        (count-classes "countClasses" :type :pointer)
                        (get-class-info "getClassInfo" :type :pointer)
                        (create-instance "createInstance" :type :pointer)
                        (get-class-info2 "getClassInfo2" :type :pointer))

(cstruct-and-class-item iplugin-factory2 "Steinberg_IPluginFactory2"
                        (vtbl "lpVtbl" :type (:pointer (:struct iplugin-factory2-vtbl))))

(cstruct-and-class-item iplugin-factory3-vtbl "Steinberg_IPluginFactory3Vtbl"
                        (query-interface "queryInterface" :type :pointer)
                        (add-ref "addRef" :type :pointer)
                        (release "release" :type :pointer)
                        (get-factory-info "getFactoryInfo" :type :pointer)
                        (count-classes "countClasses" :type :pointer)
                        (get-class-info "getClassInfo" :type :pointer)
                        (create-instance "createInstance" :type :pointer)
                        (get-class-info2 "getClassInfo2" :type :pointer)
                        (get-class-info-unicode "getClassInfoUnicode" :type :pointer)
                        (set-host-context "setHostContext" :type :pointer))

(cstruct-and-class-item iplugin-factory3 "Steinberg_IPluginFactory3"
                        (vtbl "lpVtbl" :type (:pointer (:struct iplugin-factory3-vtbl))))

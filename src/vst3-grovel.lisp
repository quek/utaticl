(in-package :vst3)

(include "c:/Users/ancient/quicklisp/local-projects/dgw/lib/vst3_c_api/vst3_c_api.h")

(ctype tresult "Steinberg_tresult")

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

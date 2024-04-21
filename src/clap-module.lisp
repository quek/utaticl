(in-package :dgw)

(cffi:defcallback get-extension :pointer
    ((host :pointer)
     (extension-id :string))
  (declare (ignore host extension-id))
  (cffi:null-pointer))

(cffi:defcallback request-restart :void
    ((host :pointer))
  (declare (ignore host)))

(cffi:defcallback request-process :void
    ((host :pointer))
  (declare (ignore host)))

(cffi:defcallback request-callback :void
    ((host :pointer))
  (declare (ignore host)))


(alexandria:define-constant +host-name+ "DGW" :test 'equal)
(alexandria:define-constant +host-url+ "https://github.com/quek/dgw" :test 'equal)
(alexandria:define-constant +host-version+ "0.0.1" :test 'equal)

(sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
  (autowrap:with-alloc (host '(:struct (clap:clap-host)))
    (setf (clap:clap-host.clap-version.major host) 1
          (clap:clap-host.clap-version.minor host) 1
          (clap:clap-host.clap-version.revision host) 10)
    (setf (clap:clap-host.name host) (sb-sys:vector-sap +host-name+))
    (setf (clap:clap-host.vendor host) (sb-sys:vector-sap +host-name+))
    (setf (clap:clap-host.url host) (sb-sys:vector-sap +host-url+))
    (setf (clap:clap-host.version host) (sb-sys:vector-sap +host-version+))
    (setf (clap:clap-host.get-extension host) (cffi:callback get-extension))
    (setf (clap:clap-host.request-restart host) (cffi:callback request-restart))
    (setf (clap:clap-host.request-process host) (cffi:callback request-process))
    (setf (clap:clap-host.request-callback host) (cffi:callback request-callback))
    
    (let* ((path "c:/Program Files/Common Files/CLAP/Surge Synth Team/Surge XT.clap")
           (lib (cffi:foreign-funcall "LoadLibraryA" :string path :pointer))
           (clap-entry (cffi:foreign-funcall "GetProcAddress" :pointer lib :string "clap_entry" :pointer))
           (clap-entry (clap::make-clap-plugin-entry :ptr clap-entry))
           (init (clap:clap-plugin-entry.init clap-entry)))
      (cffi:foreign-funcall-pointer init ()
                                    :string path
                                    :bool)
      (let* ((factory (cffi:foreign-funcall-pointer 
                       (clap:clap-plugin-entry.get-factory clap-entry) ()
                       :string "clap.plugin-factory"
                       :pointer))
             (factory (clap::make-clap-plugin-factory :ptr factory))
             #+nil
             (plugin-count (cffi:foreign-funcall-pointer
                            (clap:clap-plugin-factory.get-plugin-count factory) ()
                            :pointer (autowrap:ptr factory)
                            :unsigned-int))
             (desc (clap::make-clap-plugin-descriptor
                    :ptr (cffi:foreign-funcall-pointer
                          (clap:clap-plugin-factory.get-plugin-descriptor factory) ()
                          :pointer (autowrap:ptr factory)
                          :unsigned-int 0
                          :pointer)))

             (plugin (cffi:foreign-funcall-pointer
                      (clap:clap-plugin-factory.create-plugin factory) ()
                      :pointer (autowrap:ptr factory)
                      :pointer (autowrap:ptr host)
                      :string (clap:clap-plugin-descriptor.id desc)
                      :pointer)))
        plugin))))

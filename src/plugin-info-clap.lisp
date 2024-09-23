(in-package :utaticl.core)

(defmethod initialize-instance :after ((plugin-info-clap plugin-info-clap)
                                       &key descriptor)
  (when descriptor
    (setf (.id plugin-info-clap)
          (cffi:foreign-string-to-lisp
           (clap:clap-plugin-descriptor.id descriptor)))
    (setf (.name plugin-info-clap)
          (cffi:foreign-string-to-lisp
           (clap:clap-plugin-descriptor.name descriptor)))))

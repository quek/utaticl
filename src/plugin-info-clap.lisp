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

(defmethod api ((plugin-info-clap plugin-info-clap))
  "clap")

(defmethod plugin-load ((plugin-info-clap plugin-info-clap))
  (multiple-value-bind (factory library)
      (utaticl.clap::get-factory (.path plugin-info-clap))
    (make-instance 'module-clap
                   :id (.id plugin-info-clap)
                   :name (.name plugin-info-clap)
                   :library library
                   :factory factory
                   )))

(in-package :utaticl.core)

(defmethod initialize-instance :after ((self param-clap) &key clap-param-info)
  (when clap-param-info
    (setf (.id self) (clap:clap-param-info.id clap-param-info))
    (setf (.name self)
          (format nil "~a/~a"
                  (cffi:foreign-string-to-lisp (clap:clap-param-info.module[]& clap-param-info))
                  (cffi:foreign-string-to-lisp (clap:clap-param-info.name[]& clap-param-info))))
    (setf (.short-title self)
          (cffi:foreign-string-to-lisp (clap:clap-param-info.name[]& clap-param-info)))
    (setf (.default-value self)
          (clap:clap-param-info.default-value clap-param-info))
    (setf (.max-value self)
          (clap:clap-param-info.max-value clap-param-info))
    (setf (.min-value self)
          (clap:clap-param-info.min-value clap-param-info))
    (setf (.flags self)
          (clap:clap-param-info.flags clap-param-info))))

(defmethod automate-p ((self param-clap))
  (plusp (logand (.flags self)
                 clap:+clap-param-is-automatable+)))

(defmethod value-changed-by-host ((self param-clap))
  (param-change-add (.module self) self))

(defmethod value-text ((self param-clap))
  (let ((param self)
        (self (.module self))
        (buffer-size 80))
    (cffi:with-foreign-object (buffer :char buffer-size)
      (utaticl.clap::ecall
       (clap:clap-plugin-params.value-to-text (.ext-params self))
       :unsigned-int (.id param)
       :double (.value param)
       :pointer buffer
       :unsigned-int buffer-size
       :bool)
      (cffi:foreign-string-to-lisp buffer))))

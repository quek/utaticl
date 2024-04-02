(in-package :vst3)

(defmacro call (this method &rest args)
  (let ((self (gensym "SELF"))
        (method-pointer (gensym "METHOD-POINTER")))
    `(let* ((,self ,this)
            (,method-pointer (funcall
                              (symbol-function
                               (intern
                                (format nil "~a~a-VTBL-~a"
                                        (if (eq (type-of ,self) 'unknown)
                                            "F" "I")
                                        (type-of ,self)
                                        ',method)
                                :vst3))
                              (.vtbl ,self))))
       (cffi:foreign-funcall-pointer
        ,method-pointer ()
        :pointer (.ptr ,self)
        ,@args))))

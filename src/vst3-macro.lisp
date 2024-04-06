(in-package :vst3)

(defmacro call (this method &rest args)
  (let ((self (gensym "SELF"))
        (method-pointer (gensym "METHOD-POINTER")))
    `(let* ((,self ,this)
            (,method-pointer (funcall
                              (symbol-function
                               (intern
                                (format nil "STEINBERG-~a.LP-VTBL*.~a"
                                        (type-of ,self)
                                        ',method)
                                :vst3-c-api))
                              (.instance ,self))))
       (cffi:foreign-funcall-pointer
        ,method-pointer ()
        :pointer (funcall (symbol-function
                           (intern (format nil
                                           "STEINBERG-~a-PTR"
                                           (type-of self))
                                   :vst3-c-api))
                          (.instance self))
        ,@args))))

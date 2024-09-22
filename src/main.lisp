(in-package :utaticl)

(cffi:load-foreign-library "ole32.dll")

(cffi:defcfun ("OleInitialize" ole-initialize) :long
  (x :pointer))

(cffi:defcfun ("OleUninitialize" ole-uninitialize) :void)

(defmacro with-ole (&body body)
  `(progn
     (ole-initialize (cffi:null-pointer))
     (unwind-protect (progn ,@body)
       (ole-uninitialize))))

(defun main ()
  (sb-thread:make-thread
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
       (with-ole
         (pa:with-audio
           (utaticl.core:with-thraed-pool
             (setf utaticl.core:*app* (make-instance 'utaticl.core:app :backend
                                                     ;;:glfw-opengl3
                                                     :sdl-vulkan
                                                     ))
             (unwind-protect
                  (utaticl.core:run-with-backend
                   utaticl.core:*app*
                   (utaticl.core:.backend utaticl.core:*app*))
               (utaticl.core:terminate utaticl.core:*app*)))))))
   :name "UTATICL"))

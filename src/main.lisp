(in-package :dgw)

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
         (with-thraed-pool
           ;; (vulkan-backend::vulkan-backend-main)
           (dgw.glfw-opengl3::main)
           ))))
   :name "DGW"))

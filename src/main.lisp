(in-package :dgw)

(defun main ()
  (sb-thread:make-thread
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
       (portaudio:with-audio
         (with-thraed-pool
           (setf *app* (make-instance 'app))
           (vulkan-backend::vulkan-backend-main *app*)
           (terminate *app*)))))
   :name "DGW"))

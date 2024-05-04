(in-package :dgw)

(defun main ()
  (sb-thread:make-thread
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
       (setf *app* (make-instance 'app))
       (with-audio
         (start-audio)
         (vulkan-backend::vulkan-backend-main *app*)
         (stop-audio)
         (terminate *app*))))
   :name "DGW"))

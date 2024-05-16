(in-package :dgw)

(defun main ()
  (sb-thread:make-thread
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
       (with-thraed-pool
         (setf *app* (make-instance 'app))
         (setf *config* (make-instance 'config))
         (config-load *config*)
         (setf *theme* (make-instance 'theme))
         (config-load *theme*)
         (with-audio
           (start-audio)
           (vulkan-backend::vulkan-backend-main *app*)
           (stop-audio)
           (terminate *app*)))))
   :name "DGW"))

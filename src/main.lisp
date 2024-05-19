(in-package :dgw)

(defun main ()
  (sb-thread:make-thread
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
       (with-thraed-pool
         (vulkan-backend::vulkan-backend-main))))
   :name "DGW"))

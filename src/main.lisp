(in-package :dgw)

(defun main ()
  (sb-thread:make-thread
   (lambda ()
     (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
       (setf *app* (make-instance 'app))
       (with-audio
         (start-audio)
         (sdl2-main *app*)
         (terminate *app*))))
   :name "DGW"))

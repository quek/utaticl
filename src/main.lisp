(in-package :dgw)

(defun main ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
    (setf *app* (make-instance 'app))
    (with-audio
      (sdl2-main *app*)
      (terminate *app*)
      (stop-audio))))

(in-package :dgw)

(defun main ()
  (setf *app* (make-instance 'app))
  (with-audio
    (sdl2-main *app*)
    (stop *app*)
    (stop-audio)))

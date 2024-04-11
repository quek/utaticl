(in-package :dgw)

(defun main ()
  (let ((app (make-instance 'app)))
    (sdl2-main app)))

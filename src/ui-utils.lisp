(in-package :dgw)

(defun @ (x y)
    (list x y))

(defun @+ (&rest vec2-list)
  (labels ((add (&rest args)
             (if (endp args)
                 (list 0.0 0.0)
                 (let ((car (car args))
                       (cdr (apply #'add (cdr args))))
                   (list (+ (car car) (car cdr))
                         (+ (cadr car) (cadr cdr)))))))
    (apply #'add vec2-list)))

(defmethod .x ((self list))
  (car self))

(defmethod .y ((self list))
  (cadr self))

(defun color (r g b &optional (a #x80))
  (+ (* a #x1000000)
     (* b #x10000)
     (* g #x100)
     r))

(defmacro defshortcut (key-chord &body body)
  `(progn
     (ig:set-next-item-shortcut ,key-chord)
     (ig:push-id)
     (when (ig:button "##_" (@ -1.0 -1.0))
       ,@body)
     (ig:pop-id)))

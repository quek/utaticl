(in-package :dgw)

(defmacro defserialize (class &rest slots)
  `(progn
     (defmethod serialize ((self ,class))
       (list (class-name (class-of self))
             ,@(loop for slot in slots
                     nconc `(',slot (slot-value self ',slot)))))
     (defmethod deserialize-slots ((self ,class) sexp)
       (loop for (slot value) on sexp by #'cddr
             ,@(loop for slot in slots
                     nconc `(if (eq slot ',slot)
                                do (setf (slot-value self ',slot) value))))
       self)))

(defmethod deserialize (sexp)
  (deserialize-slots (make-instance (car sexp)) (cdr sexp)))

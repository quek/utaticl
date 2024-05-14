(in-package :dgw)

(defmacro defserialize (class &rest slots)
  `(progn
     (defmethod serialize-slots ((self ,class))
       `(,,@(loop for slot in slots
                  nconc `(',slot (serialize (slot-value self ',slot))))
         ,@(call-next-method)))
     (defmethod deserialize-slots ((self ,class) slot value)
       (if (member slot ',slots)
           (setf (slot-value self slot) (deserialize value))
           (call-next-method)))))

(defmacro with-serialize-context (&body body)
  `(let ((*serialize-context* (make-instance 'serialize-context)))
     ,@body))

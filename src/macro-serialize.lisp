(in-package :dgw)

(defvar *serialize-context*)
(defconstant +neko-ref+ '+neko-ref+)

(defmacro defserialize (class &rest slots)
  `(progn
     (defmethod serialize-slots ((self ,class))
       `(,,@(loop for slot in slots
                  if (and (consp slot) (eq +neko-ref+ (car slot)))
                    nconc `(',(cadr slot) (.neko-id (slot-value self ',(cadr slot))))
                  else
                    nconc `(',slot (serialize (slot-value self ',slot))))
         ,@(call-next-method)))

     (defmethod deserialize-slot ((self ,class) slot value)
       (if (member slot ',slots)
           (setf (slot-value self slot) (deserialize value))
           (if (member (list +neko-ref+ slot) ',slots :test #'equal)
               (after-add *serialize-context*
                          (lambda ()
                            (setf (slot-value self slot) (if (atom value)
                                                             (find-neko value)
                                                             (mapcar #'find-neko value)))))
               (call-next-method))))))

(defmacro with-serialize-context (&body body)
  `(let ((*serialize-context* (make-instance 'serialize-context)))
     (prog1 (progn ,@body)
       (serialize-context-finalize))))

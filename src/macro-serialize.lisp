(in-package :dgw)

(defvar *serialize-context*)

(defmacro defserialize (class &rest specs)
  `(progn
     (defmethod serialize-slots ((self ,class))
       `(,,@(loop for slot in specs
                  if (and (consp slot) (eq :ref (car slot)))
                    nconc `(',(cadr slot) (.neko-id (slot-value self ',(cadr slot))))
                  else if (and (consp slot) (eq :list (car slot)))
                         nconc `(',(cadr slot) (serialize (slot-value self ',(cadr slot))))
                  else
                    nconc `(',slot (serialize (slot-value self ',slot))))
         ,@(call-next-method)))

     (defmethod deserialize-slot ((self ,class) slot value)
       (cond
         ,@(loop for spec in specs
                 collect
                 (cond ((atom spec)
                        `((eq slot ',spec)
                          ,(let ((fsym (find-symbol (format nil ".~a" spec) :dgw)))
                             (if (fboundp fsym)
                                 `(setf (,fsym self) (deserialize value))
                                 `(setf (slot-value self slot) (deserialize value))))))
                       ((eq :ref (car spec))
                        `((eq slot ',(cadr spec))
                          (after-add *serialize-context*
                                     (lambda ()
                                       (setf (slot-value self slot)
                                             (if (atom value)
                                                 (find-neko value)
                                                 (mapcar #'find-neko value)))))))
                       ((eq :list (car spec))
                        `((eq slot ',(cadr spec))
                          (setf (slot-value self slot) nil)
                          (loop for x in (deserialize value)
                                do (,(getf spec :writer) self x))))))
         (t (call-next-method))))))

(defmacro with-serialize-context (&body body)
  `(let ((*serialize-context* (make-instance 'serialize-context)))
     (prog1 (progn ,@body)
       (serialize-context-finalize))))

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
                  else if (and (consp slot) (eq :hash (car slot)))
                         nconc `(',(cadr slot)
         (loop for key being the hash-key in (slot-value self ',(cadr slot))
                 using (hash-value val)
               collect (list (serialize key) (serialize val))))
                  else
                    nconc `(',slot (serialize (slot-value self ',slot))))
         ,@(call-next-method)))

     (defmethod deserialize-slot ((self ,class) slot value)
       (cond
         ,@(loop for spec in specs
                 collect
                 (cond ((atom spec)
                        `((eq slot ',spec)
                          ,(let ((accessor (find-symbol (format nil ".~a" spec) :dgw)))
                             `(if (and (consp value) (eq :ref (car value)))
                                  (after-add *serialize-context*
                                             (lambda ()
                                               (setf ,(if (fboundp accessor)
                                                          `(,accessor self)
                                                          `(slot-value self slot))
                                                     (find-neko (cadr value)))))
                                  (setf ,(if (fboundp accessor)
                                             `(,accessor self)
                                             `(slot-value self slot))
                                        (deserialize value))))))
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
                                do (,(getf spec :writer) self x))))
                       ((eq :hash (car spec))
                        `((eq slot ',(cadr spec))
                          (loop for (key val) in value
                                for key-ref-p = (and (consp key) (eq :ref (car key)))
                                for val-ref-p = (and (consp val) (eq :ref (car val)))
                                if (or key-ref-p val-ref-p)
                                  do (after-add *serialize-context*
                                                (let ((key key)
                                                      (val val)
                                                      (key-ref-p key-ref-p)
                                                      (val-ref-p val-ref-p))
                                                  (lambda ()
                                                    (,(getf spec :writer)
                                                     self
                                                     (if key-ref-p
                                                         (find-neko (cadr key))
                                                         (deserialize key))
                                                     (if val-ref-p
                                                         (find-neko (cadr val))
                                                         (deserialize val))))))
                                else
                                  do (,(getf spec :writer)
                                      self
                                      (deserialize key)
                                      (deserialize val)))))))
         (t (call-next-method))))))

(defmacro with-serialize-context ((&key copy) &body body)
  `(let ((*serialize-context* (make-instance 'serialize-context :copy ,copy)))
     (prog1 (progn ,@body)
       (serialize-context-finalize))))

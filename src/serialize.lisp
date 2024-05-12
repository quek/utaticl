(in-package :dgw)

(defmacro defserialize (class &rest slots)
  `(progn
     (defmethod serialize-slots ((self ,class))
       `(,,@(loop for slot in slots
                  nconc `(',slot (serialize (slot-value self ',slot))))
         ,@(call-next-method)))
     (defmethod deserialize-slots ((self ,class) slot value)
       (if (member slot ',slots)
           (setf (slot-value self slot) value)
           (call-next-method)))))

(defmethod deserialize (sexp)
  (cond ((atom sexp)
         sexp)
        ((eq 'list (car sexp))
         (loop for x in (cdr sexp)
               collect (deserialize x)))
        ((eq 'hash-table (car sexp))
         (let ((map (make-hash-table :test (cadr sexp))))
           (loop for (key value) on (cddr sexp) by #'cddr
                 do (setf (gethash (deserialize key) map)
                          (deserialize value)))))
        (t
         (let ((self (make-instance (car sexp))))
           (loop for (slot value) on (cdr sexp) by #'cddr
                 do (deserialize-slots self slot (deserialize value)))
           self))))

(defmethod serialize ((self t))
  self)

(defmethod serialize ((self standard-object))
  `(,(class-name (class-of self))
    ,@(serialize-slots self)))

(defmethod serialize ((self list))
  `(list ,@(loop for x in self
                 collect (serialize x))))

(defmethod serialize ((self hash-table))
  `(hash-table ',(hash-table-test self)
               ,@ (let (xs)
                    (maphash (lambda (key value)
                               (push (list (serialize key) (serialize value))
                                     xs))
                             self)
                    xs)))

(defmethod serialize-slots ((self t))
  nil)

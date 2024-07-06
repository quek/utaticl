(in-package :dgw)

(defclass serialize-context ()
  ((afters :initform nil :accessor .afters)
   (copy :initarg :copy :initform nil :accessor .copy)
   (map :initform (make-hash-table) :accessor .map)))

(defmethod after-add ((self serialize-context) f)
  (push f (.afters self)))

(defmethod deserialize (sexp)
  (aprog1 (cond ((atom sexp)
                 sexp)
                ((eq 'list (car sexp))
                 (let ((list (loop repeat (length (cdr sexp))
                                   collect nil)))
                   (loop for x in (cdr sexp)
                         for i from 0
                         if (and (consp x) (eq :ref (car x)))
                           do (after-add *serialize-context*
                                         (let ((i i)
                                               (neko-id (cadr x)))
                                           (lambda ()
                                             (setf (nth i list) (find-neko neko-id)))))
                         else
                           do (setf (nth i list) (deserialize x)))
                   list))
                (t
                 (deserialize-neko (make-instance (car sexp)) (cdr sexp))))
    (deserialize-after it)))

(defmethod deserialize-after (self))

(defmethod deserialize-neko (instance slots)
  (let ((self instance))
    (loop for (slot value) on slots by #'cddr
          do (deserialize-slot self slot value))
    self))

(defmethod deserialize-slot (self slot value))

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

(defun serialize-context-finalize ()
  (loop for f in (.afters *serialize-context*)
        do (funcall f)))

#+nil
(sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
  (with-open-file (in (merge-pathnames "user/project/20240511.lisp" *working-directory*))
    (with-serialize-context ()
      (deserialize (read in)))))

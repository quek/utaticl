(in-package :dgw)

(defclass serialize-context ()
  ((afters :initform nil :accessor .afters)
   (map :initform (make-hash-table) :accessor .map)))

(defmethod after-add ((self serialize-context) f)
  (push f (.afters self)))

(defmethod serialize-did-p ((self serialize-context) x)
  (multiple-value-bind (value present-p) (gethash x (.map self))
    (declare (ignore value))
    present-p))

(defmethod serialize-did ((self serialize-context) x)
  (setf (gethash x (.map self)) x))

(defun serialize-context-finalize ()
  (loop for f in (.afters *serialize-context*)
        do (funcall f)))

(defmethod deserialize (sexp)
  (aprog1 (cond ((atom sexp)
                 sexp)
                ((eq 'list (car sexp))
                 (loop for x in (cdr sexp)
                       collect (deserialize x)))
                ((eq +neko-ref+ (car sexp))
                 (cadr sexp))
                ((eq 'hash-table (car sexp))
                 (let ((map (make-hash-table :test (cadr sexp))))
                   (loop for (key value) on (cddr sexp) by #'cddr
                         do (setf (gethash (deserialize key) map)
                                  (deserialize value)))))
                (t
                 (let ((self (make-instance (car sexp))))
                   (loop for (slot value) on (cdr sexp) by #'cddr
                         do (deserialize-slot self slot value))
                   self)))
    (deserialize-after it)))

(defmethod deserialize-after (self))

(defmethod deserialize-slot ((self t) slot value))

(defmethod serialize :around ((self neko))
  (if (serialize-did-p *serialize-context* self)
      `(+neko-ref+ ,(.neko-id self))
      (progn
        (serialize-did *serialize-context* self)
        (call-next-method))))

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

#+nil
(sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
  (with-open-file (in (merge-pathnames "user/project/20240511.lisp" *working-directory*))
    (deserialize (read in))))

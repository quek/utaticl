(in-package :dgw)

(defvar *serialize-context*)
(defconstant +neko-ref+ '+neko-ref+)

(defclass serialize-context ()
  ((map :initform (make-hash-table) :accessor .map)))

(defmethod serialize-did-p ((self serialize-context) x)
  (multiple-value-bind (value present-p) (gethash x (.map self))
    (declare (ignore value))
    present-p))

(defmethod serialize-did ((self serialize-context) x)
  (setf (gethash x (.map self)) x))


(defmethod deserialize (sexp)
  (aprog1 (cond ((atom sexp)
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
                         do (deserialize-slots self slot value))
                   self)))
    (deserialize-after it)))

(defmethod deserialize-after (self))

(defmethod deserialize-slots ((self t) slot value))

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

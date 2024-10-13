(in-package :utaticl.core)

(defvar *neko-map* (make-hash-table :weakness :value :test 'equal
                                    :synchronized t))

(defun find-neko (neko-id)
  (gethash neko-id *neko-map*))

(defmethod initialize-instance :after ((self neko) &key)
  (sb-ext:with-locked-hash-table (*neko-map*)
    (unless (slot-boundp self 'neko-id)
      (loop for uid = (uid)
            if (gethash uid *neko-map*)
              do (break "same uid generated ~a" uid)
            unless (gethash uid *neko-map*)
              do (setf (slot-value self 'neko-id) uid)
                 (loop-finish)))
    (setf (gethash (slot-value self 'neko-id) *neko-map*) self)))

(defmethod copy ((self neko))
  (with-serialize-context (:copy t)
    (deserialize (with-serialize-context ()
                   (serialize self)))))

(defmethod dd-show ((self neko))
  (ig:text (.name self)))

(defmethod deserialize-slot ((self neko) (slot (eql 'neko-id)) value)
  (if (.copy *serialize-context*)
      (setf (.neko-id self) (uid))
      (call-next-method)))

(assert (let ((self (make-instance 'neko)))
          (string/= (.neko-id self)
                    (.neko-id (copy self)))))

(defun name-new (class-symbol prefix)
  (sb-ext:with-locked-hash-table (*neko-map*)
    (format
     nil (format nil "~a~~d" prefix)
     (1+
      (loop for neko being the hash-value in *neko-map*
            maximize
            (or
             (and (typep neko class-symbol)
                  (ppcre:register-groups-bind
                      ((#'parse-integer n))
                      ((format nil "^~a(\\d+)$" prefix) (.name neko))
                    n))
             0))))))

(defmethod (setf .neko-id) :around (value (self neko))
  (sb-ext:with-locked-hash-table (*neko-map*)
    (let ((neko-id-old (.neko-id self)))
      (prog1 (call-next-method)
        (remhash neko-id-old *neko-map*)
        (setf (gethash value *neko-map*) self)))))

(defmethod .neko-id ((self list))
  (mapcar #'.neko-id self))

(defmethod print-object ((self neko) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a"
            (.name self)
            (.neko-id self))))

(defmethod ig:push-id ((self neko))
  (ig:push-id (.neko-id self)))

(defmethod serialize :around ((self neko))
  (if (gethash self (.map *serialize-context*))
      `(:ref ,(.neko-id self))
      (progn
        (setf (gethash self (.map *serialize-context*)) self)
        (call-next-method))))

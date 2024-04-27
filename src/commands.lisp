(in-package :dgw)

(defmacro defcommand (name super slots &optional class-options)
  `(defclass ,name ,super
     ,slots
     ,@(when class-options `(, class-options))))

(defclass command ()
  ((undo-p :initarg :undo-p :initform t :accessor .undo-p)))

(defmethod redo ((self command))
  (execute self))

(defcommand cmd-redo (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-redo))
  (cmd-redo *project*))

(defcommand cmd-track-add (command)
  ())

(defmethod execute ((self cmd-track-add))
  (setf (.tracks (.master-track *project*))
        (append (.tracks (.master-track *project*))
                (list (make-instance 'track)))))

(defmethod undo ((self cmd-track-add))
  (setf (.tracks (.master-track *project*))
        (butlast (.tracks (.master-track *project*)))))

(defcommand cmd-undo (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-undo))
  (cmd-undo *project*))

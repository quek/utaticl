(in-package :dgw)

(defmacro defcommand (name super slots &optional class-options)
  `(defclass ,name ,super
     ,slots
     ,@(when class-options `(, class-options))))

(defclass command ()
  ((undo-p :initarg :undo-p :initform t :accessor .undo-p)))

(defmethod redo ((self command))
  (execute self))

(defcommand cmd-plugin-add (command)
  ((track-id :initarg :track-id :accessor .track-id)
   (plugin-info :initarg :plugin-info :accessor .plugin-info)))

(defmethod execute ((self cmd-plugin-add))
  (let ((track (find-track *project* (.track-id self)))
        (module (plugin-load (.plugin-info self))))
    (module-add track module)))

(defcommand cmd-plugin-scan (command)
  ())

(defmethod execute ((self cmd-plugin-scan))
  (let ((path (merge-pathnames "user/config/plugins.lisp" *working-directory*)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (loop for plugin-info in (vst3::plugin-scan-vst3)
            do (write (serialize plugin-info) :stream out)
               (terpri out)))))

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
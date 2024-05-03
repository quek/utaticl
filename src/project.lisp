(in-package :dgw)

(defmethod initialize-instance :after ((self project) &key)
  (setf (.target-track self) (.master-track self)))

(defun cmd-add (project cmd-class &rest args)
  (push (apply #'make-instance cmd-class args) (.cmd-queue project)))

(defmethod cmd-redo ((self project))
  (let ((*project* self)
        (cmd (pop (.cmd-redo-stack self))))
    (when cmd
      (redo cmd)
      (push cmd (.cmd-undo-stack self)))))

(defmethod cmd-run ((self project))
  (let ((*project* self))
    (loop for cmd in (nreverse (.cmd-queue self))
          do (execute cmd)
             (when (.undo-p cmd)
               (setf (.cmd-redo-stack self) nil)
               (push cmd (.cmd-undo-stack self))))
    (setf (.cmd-queue self) nil)))

(defmethod cmd-undo ((self project))
  (let ((*project* self)
        (cmd (pop (.cmd-undo-stack self))))
    (when cmd
      (undo cmd)
      (push cmd (.cmd-redo-stack self)))))

(defun find-track (project track-id)
  (labels ((f (track)
             (if (equal track-id (.neko-id track))
                 track
                 (some #'f (.tracks track)))))
    (f (.master-track project))))

(defmethod render ((self project))
  (let ((*project* self))
    (render (.transposer self))
    (render (.arrangement self))
    (render (.rack self))
    (render (.commander self))
    (cffi:with-foreign-object (openp :bool)
      (setf (cffi:mem-ref openp :bool) t)
      (when (ig::begin "Hello" openp 0)
        (ig::text (format nil "Hello ~a ~a." (lisp-implementation-type) (lisp-implementation-version)))
        (when (ig::button "さようなら" '(200.0 40.0))
          (setf *done* t)))
      (ig::end))))

(defmethod process ((self project))
  (process (.master-track self)))

(defmethod terminate ((self project))
  (terminate (.master-track self)))

(defmethod unselect-all-tracks ((self project))
  (unselect-all-tracks (.master-track self)))

(in-package :dgw)

(defmethod render ((self project))
  (let ((*project* self))
    (render (.transposer self))
    (render (.arrangement self))
    (cffi:with-foreign-object (openp :bool)
      (setf (cffi:mem-ref openp :bool) t)
      (when (ig::begin "Hello" openp 0)
        (ig::text (format nil "Hello ~a ~a." (lisp-implementation-type) (lisp-implementation-version)))
        (when (ig::button "Load & Open" '(100.0 35.0))
          (open-vst3-module self))
        (when (ig::button "さようなら" '(200.0 40.0))
          (setf *done* t)))
      (ig::end))))

(defmethod open-vst3-module ((self project))
  (print 'open-vst3-module)
  (start-audio)
  (let ((module (vst3-module-load
                 "c:/Program Files/Common Files/VST3/Dexed.vst3"
                 ;;"c:/Program Files/Common Files/VST3/DS Thorn.vst3"
                 ;;"c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3"
                 ;;"c:/Program Files/Common Files/VST3/Kilohearts/Phase Plant.vst3"
                 ;;"c:/Program Files/Common Files/VST3/DS Thorn.vst3"
                 ;;"c:/Program Files/Common Files/VST3/Vital.vst3"
                 )))
    (setf (.module self) module)
    (initialize module)
    (start module)
    (editor-open module)))

(defmethod process ((self project))
  (awhen (.module self)
    (when (.start-p it)
      (process it))))

(defun cmd-add (project cmd-class &rest args)
  (push (apply #'make-instance cmd-class args) (.cmd-queue project)))

(defmethod cmd-run ((self project))
  (let ((*project* self))
    (setf (.cmd-redo-stack self) nil)
    (loop for cmd in (nreverse (.cmd-queue self))
          do (execute cmd)
             (when (.undo-p cmd)
               (push cmd (.cmd-undo-stack self))))
    (setf (.cmd-queue self) nil)))

(defmethod cmd-undo ((self project))
  (let ((*project* self)
        (cmd (pop (.cmd-undo-stack self))))
    (when cmd
      (undo cmd)
      (push cmd (.cmd-redo-stack self)))))

(defmethod terminate ((self project))
  (let ((module (.module self)))
    (when module
      (terminate module))))

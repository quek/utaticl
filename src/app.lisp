(in-package :dgw)

(defclass app ()
  ((module :initform nil :accessor .module)))

(defmethod render ((self app))
  (cffi:with-foreign-object (openp :bool)
    (setf (cffi:mem-ref openp :bool) t)
    (when (ig::begin "Hello" openp 0)
      (ig::text (format nil "Hello ~a ~a." (lisp-implementation-type) (lisp-implementation-version)))
      (when (c-with ((size ig:im-vec2))
              (setf (c-ref size ig:im-vec2 :x) 100.0)
              (setf (c-ref size ig:im-vec2 :y) 35.0)
              (/= 0 (ig::button "Load & Open" size)))
        (open-vst3-module))
      (when (c-with ((size ig:im-vec2))
              (setf (c-ref size ig:im-vec2 :x) 200.0)
              (setf (c-ref size ig:im-vec2 :y) 40.0)
              (/= 0 (ig::button "さようなら" size)))
        (setf *done* t)))
    (ig::end)))

(defmethod stop ((self app))
  (awhen (.module self)
    (terminate it)))

(defmethod open-vst3-module ()
  (print 'open-vst3-module)
  (start-audio)
  (let ((module (vst3-module-load
                 ;;"c:/Program Files/Common Files/VST3/Dexed.vst3"
                 ;;"c:/Program Files/Common Files/VST3/DS Thorn.vst3"
                 ;;"c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3"
                 ;;"c:/Program Files/Common Files/VST3/Kilohearts/Phase Plant.vst3"
                 ;;"c:/Program Files/Common Files/VST3/DS Thorn.vst3"
                 "c:/Program Files/Common Files/VST3/Vital.vst3"
                 )))
    (setf (.module *app*) module)
    (initialize module)
    (start module)
    (editor-open module)
    ))

(defmethod process ((self app))
  (awhen (.module self)
    (when (.start-p it)
      (process it))))

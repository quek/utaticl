(in-package :dgw)

(defclass transposer ()
  ())

(defmethod render ((self transposer) context)
  (print (.bpm *project*))
  (when (ig:begin "##transponser")
    (when (ig:button "▶")
      (play self))
    (ig:same-line)
    (when (ig:button "■")
      (stop self))
    (ig:same-line)
    (ig:set-next-item-width (* (ig:get-font-size) 4))
    (ig:drag-float "BPM" (.bpm *project*)))

  (ig:end))

(defmethod play ((self transposer))
  (setf (.playing-p *project*) t))

(defmethod stop ((self transposer))
  (setf (.playing-p *project*) nil))

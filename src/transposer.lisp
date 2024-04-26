(in-package :dgw)

(defmethod render ((self transposer))
  (when (ig:begin "##transponser")
    (when (ig:button "▶")
      (play self))
    (ig:same-line)
    (when (ig:button "■")
      (stop self))
    (ig:same-line)
    (ig:set-next-item-width (* (ig:get-font-size) 3))
    (ig:drag-float "BPM" (.bpm *project*) :format "%.2f"))

  (ig:end))

(defmethod play ((self transposer))
  (setf (.playing-p *project*) t))

(defmethod stop ((self transposer))
  (setf (.playing-p *project*) nil))

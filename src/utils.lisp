(in-package :utaticl.core)

(defun fuzzy= (x y)
  (let ((end (length y)))
    (if (zerop end)
        t
        (loop with yi = 0
              for xc across x
              for yc = (char-downcase (char y yi))
                thereis (and (char= (char-downcase xc) yc)
                             (= (incf yi) end))))))

(defun interval-p (sec)
  (< (mod (/ (get-internal-real-time) internal-time-units-per-second)
          (* sec 2))
     sec))

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dst :pointer)
  (src :pointer)
  (size :size))

(defun uid ()
  (princ-to-string (uuid:make-v4-uuid)))

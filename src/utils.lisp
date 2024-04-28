(in-package :dgw)

(defun fuzzy= (x y)
  (let ((end (length y)))
    (if (zerop end)
        t
        (loop with yi = 0
              for xc across x
              for yc = (char-downcase (char y yi))
                thereis (and (char= (char-downcase xc) yc)
                             (= (incf yi) end))))))

(defun uid ()
  (random-uuid:to-string (random-uuid:make-uuid)))

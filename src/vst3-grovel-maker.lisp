(in-package :vst3-grovel)

(defun comma-reader (stream char)
  (declare (ignore char))
  (read stream t t t))

(defun slash-reader (stream char)
  (declare (ignore char))
  (let ((next (peek-char nil stream)))
    (cond ((eql next #\/)
           (list :comment (format nil "/~a" (read-line stream))))
          ((eql next #\*)
           (list :comment
                 (with-output-to-string (out)
                   (read-char stream)
                   (write-string "/*" out)
                   (loop for c1 = (read-char stream) then c2
                         for c2 = (read-char stream)
                         until (and (eql c1 #\*) (eql c2 #\/))
                         do (write-char c1 out)
                         finally (write-char c2 out)))))
          (t '/))))

(defun sharpe-reader (stream char)
  (declare (ignore char))
  (let ((x (read stream nil t)))
    (cond ((eql x 'pragma) (list x (read stream nil t))))))


(defun make-readtable ()
  (let ((readtable (copy-readtable nil)))
    (set-macro-character #\, 'comma-reader nil readtable)
    (set-macro-character #\# 'sharpe-reader nil readtable)
    (set-macro-character #\/ 'slash-reader nil readtable)
    readtable))

(with-open-file (in (asdf:system-relative-pathname :dgw "lib/vst3_c_api/vst3_c_api.h"))
  (let ((*readtable* (make-readtable)))
    (loop for x = (read in nil in)
          until (eq x in)
          collect x)))

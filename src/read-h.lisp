(in-package :grovel)

(defvar *statement* nil)

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
  (let ((*readtable* (make-readtable nil t)))
    (read-delimited-list #\newline stream t)))

(defun symbol-reader (stream char)
  (unread-char char stream)
  (let ((sym (let ((*readtable* (make-readtable nil nil)))
               (read stream))))
    (if *statement*
        (setf *statement* sym)
        (let ((*statement* sym))
          (cons sym (read-delimited-list #\; stream t))))))

(defun |(-reader| (stream char)
  (declare (ignore char))
  (let ((*readtable* (make-readtable nil nil)))
   (read-delimited-list #\) stream t)))

(defun |{-reader| (stream char)
  (declare (ignore char))
  (cond ((string-equal *statement* "enum")
         (read-delimited-list #\} stream t))
        (t (let ((*statement* nil))
             (read-delimited-list #\} stream t)))))

(defun |\|-reader| (stream char)
  (declare (ignore stream char))
  '|\||)

(defun \\-reader (stream char)
  (declare (ignore char))
  (read-char stream)
  (values))

(defun make-readtable (symbol-dispatch newline)
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) :preserve)
    (set-macro-character #\, 'comma-reader nil readtable)
    (set-macro-character #\| '|\|-reader| nil readtable)
    (set-macro-character #\# 'sharpe-reader nil readtable)
    (set-macro-character #\/ 'slash-reader nil readtable)
    (set-macro-character #\\ '\\-reader nil readtable)
    (set-macro-character #\{ '|{-reader| nil readtable)
    (set-macro-character #\( '|(-reader| nil readtable)
    (set-macro-character #\} (get-macro-character #\) readtable) nil readtable)
    (set-macro-character #\) (get-macro-character #\) readtable) nil readtable)
    (set-macro-character #\; (get-macro-character #\) readtable) nil readtable)
    (when newline
      (set-macro-character #\newline (get-macro-character #\) readtable) nil readtable))
    (when symbol-dispatch
      (loop for c from (char-code #\a) to (char-code #\z)
            do (set-macro-character (code-char c) 'symbol-reader t readtable))
      (loop for c from (char-code #\A) to (char-code #\Z)
            do (set-macro-character (code-char c) 'symbol-reader t readtable))
      (set-macro-character #\_ 'symbol-reader t readtable))
    
    readtable))

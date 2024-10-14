(in-package :utaticl.core)

(defun dd-at ()
  (.at *dd*))

(defmethod dd-drop (x rect)
  (if (and (dd-src)
           (typecase (car (dd-src))
             ;; 外部からの dd は ig:is-mouse-down が使えない
             (pathname (.drop-p *dd*))
             (t (not (ig:is-mouse-down ig:+im-gui-mouse-button-left+))))
           (include-p rect *mouse-pos*))
      (dd-drop-at x (car (dd-src)))
      nil))

(defmethod dd-drop-at :around (at src)
  (if (call-next-method)
      (progn
        (dd-reset)
        t)
      nil))

(defmethod dd-drop-at (at src)
  "drop を受け入れたら t を返す"
  nil)

(defun dd-drop-did ()
  (setf (.drop-p *dd*) t))

(defmethod dd-show (x)
  (ig:text (princ-to-string x)))

(defun dd-src ()
  (.src *dd*))

(defmethod dd-start ((src list) &optional at)
  (setf (.at *dd*) at)
  (setf (.src *dd*) src))

(defmethod dd-start (src &optional at)
  (dd-start (list src) at))

(defun dd-start-p ()
  (dd-src))

(defun dd-reset ()
  (setf (.at *dd*) nil)
  (setf (.src *dd*) nil)
  (setf (.drop-p *dd*) nil))

(defmethod print-object ((self dd) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a ~a" (.src self) (.at self))))

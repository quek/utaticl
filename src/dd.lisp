(in-package :utaticl.core)

(defun dd-at ()
  (.at *dd*))

(defmethod dd-drop (view dst &key rect (check-hovered-p (null rect)))
  (if (and (dd-src)
           (typecase (car (dd-src))
             ;; 外部からの dd は ig:is-mouse-down が使えない
             (pathname (.drop-p *dd*))
             (t (not (ig:is-mouse-down ig:+im-gui-mouse-button-left+))))
           (cond (rect
                  (include-p rect *mouse-pos*))
                 (check-hovered-p
                  (ig:is-item-hovered))
                 (t t)))
      (dd-drop-at view dst (car (dd-src)))
      (dd-over-at view dst (car (dd-src)))))

(defmethod dd-drop-at :around (view dst src)
  (if (call-next-method)
      (progn
        (dd-reset)
        t)
      nil))

(defmethod dd-drop-at (view dst src)
  "drop を受け入れたら t を返す"
  nil)

(defun dd-drop-did ()
  (setf (.drop-p *dd*) t))

(defmethod dd-over-at (view dst src))

(defmethod dd-show (x)
  (ig:text (princ-to-string x)))

(defun dd-src ()
  (.src *dd*))

(defmethod dd-start-force (window (src list) &optional at)
  (setf (.at *dd*) at)
  (setf (.src *dd*) src))

(defmethod dd-start-force (window src &optional at)
  (dd-start-force (list src) at))

(defmethod dd-start-here-p (window src)
  "端の方をドラッグした場合 is-item-hovered じゃなくなるので
is-item-hovered はチェックしない"
  (and (null (dd-src))
       (eq (.target *dd*) src)
       (ig:is-mouse-dragging ig:+im-gui-mouse-button-left+)))

(defmethod dd-start (window target &key (src target) at)
  (when (and (ig:is-mouse-clicked ig:+im-gui-mouse-button-left+))
    (if (ig:is-item-hovered)
        (setf (.target *dd*) target)
        (when (eq (.target *dd*) target)
          (setf (.target *dd*) nil))))
  (if (dd-start-here-p window target)
      (progn
        (dd-start-force window src at)
        t)
      nil))

(defun dd-start-p ()
  (dd-src))

(defun dd-reset ()
  (setf (.at *dd*) nil)
  (setf (.src *dd*) nil)
  (setf (.drop-p *dd*) nil))

(defmethod print-object ((self dd) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a ~a" (.src self) (.at self))))

(in-package :dgw)

(defun color-window (neko)
  (setf (.neko (.color-window *app*)) neko)
  (show (.color-window *app*)))

(defmethod hide :after ((color-window color-window))
  (setf (.neko color-window) nil))

(defmethod render ((color-window color-window))
  (when (.show-p color-window)
    (ig:open-popup "Color"))

  (ig:with-popup-modal ("Color" :open-p (.show-p color-window))
    (when (ig:button "Ok")
      (hide color-window))
    (ig:same-line)
    (when (ig:button "Cancel")
      (hide color-window))))

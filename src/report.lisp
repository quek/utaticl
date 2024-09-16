(in-package :utaticl.core)

(defvar *report-window*)

(defmethod render :around ((self report-window))
  (when (.show-p self)
    (call-next-method)))

(defmethod render ((self report-window))
  (ig:with-begin ("Report" :open-p (.show-p self))
    (ig:text-wrapped (.message self))
    (ig:separator)
    (when (ig:button "Close")
      (setf (.show-p self) nil))))

(defmethod (setf .message) :around (message (self report-window))
  (when (string/= (.message self) message)
    (setf (.show-p self) t))
  (call-next-method))

(defun report (message-format &rest args)
  (setf (.message *report-window*)
        (apply #'format nil message-format args)))

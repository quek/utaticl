(in-package :utaticl.core)

(defmethod render ((self midi-device-window))
  (ig:with-begin ("Midi Device Config")
    (when (ig:is-window-appearing)
      (setf (.midi-devices-in self) (midi-devices-in))
      (setf (.midi-devices-out self) (midi-devices-out)))
    (ig:text "Input")
    (loop for device-name in (.midi-devices-in self)
          do (let ((checked (member device-name (.midi-devices-in *config*) :test #'equal)))
               (when (ig:checkbox device-name checked)
                 (if checked
                     (progn
                       (push device-name (.midi-devices-in *config*))
                       ;; TODO open device-name)
                       )
                     (progn
                       (setf (.midi-devices-in *config*)
                             (remove device-name (.midi-devices-in *config*) :test #'equal))
                       ;; TODO close device-name
                       )))))
    (ig:separator)
    (when (ig:button "Close")
      (hide self))))

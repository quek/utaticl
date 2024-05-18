(in-package :dgw)

(defparameter *standard-sample-reates* '(44100.0d0 48000.0d0 88200.0d0 96000.0d0 192000.0d0))

(defun supported-standard-sample-reates (input-parameters output-parameters)
  (loop for sample-rate in *standard-sample-reates*
        if (zerop (cffi:foreign-funcall "Pa_IsFormatSupported"
                                        pa::p-stream-parameters input-parameters
                                        pa::p-stream-parameters output-parameters
                                        :double sample-rate
                                        :int))
          collect sample-rate))

(defmethod render :before ((self audio-device-window))
  (unless (.host-apis self)
    (setf (.host-apis self)
          (loop for i below (pa:get-host-api-count)
                collect (pa:get-host-api-info i))))
  (unless (.device-infos self)
    (setf (.device-infos self)
          (loop for i of-type fixnum below (pa:get-device-count)
                collect (pa:get-device-info i)))
    (setf (.supported-standard-sample-reates self)
          (loop for i below (pa:get-device-count)
                for device-info = (pa:get-device-info i)
                for input-parameters = (pa:make-stream-parameters)
                for output-parameters = (pa:make-stream-parameters)
                collect
                (progn
                  (setf (pa:stream-parameters-device input-parameters) i)
                  (setf (pa:stream-parameters-channel-count input-parameters)
                        (pa:device-info-max-input-channels device-info))
                  (setf (pa:stream-parameters-sample-format input-parameters) :float)
                  (setf (pa:stream-parameters-suggested-latency input-parameters) .0d0)
                  (setf (pa:stream-parameters-device output-parameters) i)
                  (setf (pa:stream-parameters-channel-count output-parameters)
                        (pa:device-info-max-output-channels device-info))
                  (setf (pa:stream-parameters-sample-format output-parameters) :float)
                  (setf (pa:stream-parameters-suggested-latency output-parameters) .0d0)
                  (cond ((and (plusp (pa:stream-parameters-channel-count input-parameters))
                              (plusp (pa:stream-parameters-channel-count output-parameters)))
                         (supported-standard-sample-reates input-parameters output-parameters))
                        ((plusp (pa:stream-parameters-channel-count input-parameters))
                         (supported-standard-sample-reates input-parameters nil))
                        (t
                         (supported-standard-sample-reates nil output-parameters))))))))


(defmethod render ((self audio-device-window))
  (ig:with-begin ("Audio Device Config")
    (when (ig:is-window-appearing)
      (setf (.api self) (.audio-device-api *config*))
      (setf (.name self) (.audio-device-name *config*))
      (setf (.sample-rate self) (.sample-rate *config*)))

    (ig:combo "API" (.api self) (mapcar #'pa:host-api-info-name (.host-apis self)))
    (ig:combo "Device" (.name self) (loop for device-info in (.device-infos self)
                                          if (equal (position (.api self) (.host-apis self)
                                                              :key #'pa:host-api-info-name)
                                                    (pa:device-info-host-api device-info))
                                            collect (pa:device-info-name device-info)))
    (ig:combo "Sample Rate"
              (.sample-rate self)
              (nth (position (.name self) (.device-infos self)
                             :key #'pa:device-info-name :test #'equal)
                   (.supported-standard-sample-reates self))
              :item-display-function (lambda (sample-rate) (format nil "~f" sample-rate)))
    (ig:separator)
    (when (ig:button "Ok")
      (setf (.audio-device-api *config*) (.api self))
      (setf (.audio-device-name *config*) (.name self))
      (setf (.sample-rate *config*) (.sample-rate self))
      (config-save *config*))))

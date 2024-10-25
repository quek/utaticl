(in-package :utaticl.core)

(defparameter *standard-sample-reates* '(44100.0d0 48000.0d0 88200.0d0 96000.0d0 192000.0d0))

(defun supported-standard-sample-reates (input-parameters output-parameters
                                         device-info-host-api device-info-name)
  (loop for sample-rate in *standard-sample-reates*
        if (ignore-errors
            (pa::is-format-supported input-parameters
                                     output-parameters
                                     sample-rate)
            t)
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
                for device-info-host-api = (pa:host-api-info-name
                                            (nth (pa:device-info-host-api device-info)
                                                 (.host-apis self)))
                for device-info-name = (pa:device-info-name device-info)
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
                         (supported-standard-sample-reates input-parameters output-parameters
                                                           device-info-host-api
                                                           device-info-name))
                        ((plusp (pa:stream-parameters-channel-count input-parameters))
                         (supported-standard-sample-reates input-parameters nil
                                                           device-info-host-api
                                                           device-info-name))
                        (t
                         (supported-standard-sample-reates nil output-parameters
                                                           device-info-host-api
                                                           device-info-name))))))))


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

    (let ((device-index (position (.name self) (.device-infos self)
                                  :key #'pa:device-info-name :test #'equal)))
      (when device-index
        (when (ig:combo "Sample Rate"
                        (.sample-rate self)
                        (nth device-index
                             (.supported-standard-sample-reates self))
                        :item-display-function (lambda (sample-rate) (format nil "~f" sample-rate)))
          (when (equal (.api self) "ASIO")
            (setf (.frames-per-buffer *config*)
                  (preferred-buffer-size device-index))))

        (ig:text (format nil "Buffer Size ~d" (.frames-per-buffer *config*)))))

    (ig:separator)

    (ig:with-disabled ((and (.audio-device *app*) (.processing (.audio-device *app*))))
      (when (ig:button "Start Audio Engine")
        (cmd-add (car (.projects *app* )) 'cmd-audio-engine-start)))
    (ig:same-line)
    (ig:with-disabled ((and (.audio-device *app*) (not (.processing (.audio-device *app*)))))
      (when (ig:button "Stop Audio Engine")
        (cmd-add (car (.projects *app* )) 'cmd-audio-engine-stop)))

    (ig:separator)

    (when (ig:button "Ok")
      (setf (.audio-device-api *config*) (.api self))
      (setf (.audio-device-name *config*) (.name self))
      (setf (.sample-rate *config*) (.sample-rate self))
      (setf (.render-audio-device-window-p *app*) nil)
      (config-save *config*)

      (awhen (.audio-device *app*)
        (setf (.device-api it) (.api self))
        (setf (.device-name it) (.name self))
        (start-audio-device it)))
    (ig:same-line)
    (when (ig:button "Close")
      (setf (.render-audio-device-window-p *app*) nil))
    (when (ig:button "Reload Devices")
      (setf (.host-apis self) nil)
      (setf (.device-infos self) nil))
    (when (and (.audio-device *app*)
               (ig:button "Open"))
      (audio-device-open *app*))
    (when (and (.audio-device *app*)
               (ig:button "Close"))
      (audio-device-close *app*))))

(defun preferred-buffer-size (device-index)
  (cffi:with-foreign-objects ((min-size :long)
                              (max-size :long)
                              (preferred-size :long)
                              (granularity :long))
    ;; https://files.portaudio.com/docs/v19-doxydocs/pa__asio_8h.html
    (cffi:foreign-funcall "PaAsio_GetAvailableBufferSizes"
                          :int device-index
                          :pointer min-size
                          :pointer max-size
                          :pointer preferred-size
                          :pointer granularity
                          :int)
    (cffi:mem-ref preferred-size :long)))

;; https://files.portaudio.com/docs/v19-doxydocs/pa__asio_8h.html
;; PaAsio_ShowControlPanel

(in-package :dgw)

(defmethod initialize-instance :after ((self app) &key)
  (setf *config* (make-instance 'config))
  (config-load *config*)
  (setf *theme* (make-instance 'theme))
  (config-load *theme*))

(defmethod cmd-run ((self app))
  (loop for project in (.projects self)
        do (cmd-run project)))

(defmethod render :around ((self app))
  (if (null (.audio-device self))
      (if (or (null (.audio-device-api *config*))
              (null (.audio-device-name *config*))
              (not (.audio-device-configured-p self)))
          (render (.audio-device-window self))
          (progn
            (setf (.audio-device self) (make-instance 'audio-device))
            (open-audio-device (.audio-device self))
            (start-audio-device (.audio-device self))))
      (call-next-method)))

(defmethod render ((self app))
  (loop for project in (.projects self)
        do (render project)))

(defmethod terminate ((self app))
  (close-audio-device (.audio-device self))
  (loop for project in (.projects self)
        do (terminate project)))

(defmethod process ((self app))
  (sb-thread:with-mutex ((.mutex self))
    (loop for project in (.projects self)
          do (process project))))

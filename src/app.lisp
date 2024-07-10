(in-package :dgw)

(defmethod initialize-instance :before ((self app) &key)
  (clrhash *neko-map*)
  (setf *config* (make-instance 'config))
  (config-load *config*)
  (setf *theme* (make-instance 'theme))
  (config-load *theme*)
  (setf *report-window* (make-instance 'report-window)))

(defmethod cmd-run ((self app))
  (loop for project in (.projects self)
        do (cmd-run project)))

(defmethod render :around ((self app))
  (if (null (.audio-device self))
      (if (or (null (.audio-device-api *config*))
              (null (.audio-device-name *config*))
              (null (.sample-rate *config*)))
          (render (.audio-device-window self))
          (progn
            ;; サンプルレートとバッファサイズが決まったのでプロジェクトが作れる
            (push (make-instance 'project) (.projects self))
            ;; オーディオデバイスを開いてオーディオイベントループを開始
            (setf (.audio-device self)
                  (make-instance 'audio-device
                                 :device-api (.audio-device-api *config*)
                                 :device-name (.audio-device-name *config*)))
            (open-audio-device (.audio-device self))
            (start-audio-device (.audio-device self))))
      (progn
        (when (.render-audio-device-window-p self)
          (render (.audio-device-window self)))
        (call-next-method))))

(defmethod render ((self app))
  (loop for project in (.projects self)
        do (render project))
  (render *report-window*))

(defmethod terminate ((self app))
  (when (.audio-device self)
    (close-audio-device (.audio-device self)))
  (loop for project in (.projects self)
        do (terminate project)))

(defmethod process ((self app))
  (sb-thread:with-mutex ((.mutex self))
    (loop for project in (.projects self)
          do (process project))))

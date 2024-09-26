(in-package :utaticl.core)

(defmethod initialize-instance :after ((self process-data)
                                       &key (audio-input-bus-count 1)
                                         (audio-output-bus-count 1))
  (setf (.inputs self) (loop repeat audio-input-bus-count
                             collect (make-instance 'audio-bus :nchannels 2)))
  (setf (.outputs self) (loop repeat audio-output-bus-count
                              collect (make-instance 'audio-bus :nchannels 2))))

(defmethod .audio-input-bus-count ((process-data process-data))
  (length (.buffer (.inputs process-data))))

(defmethod .audio-output-bus-count ((process-data process-data))
  (length (.buffer (.outputs process-data))))

(defmethod prepare ((self process-data))
  (prepare (.inputs self))
  (prepare (.outputs self))
  (prepare (.input-events self))
  (prepare (.output-events self)))

(defmethod note-off ((process-data process-data) note sample-offset)
  (note-off (.input-events process-data) note sample-offset)
  (values))

(defmethod note-off-all ((self process-data))
  (loop for (key . channel) in (.notes-on self)
        do (note-off self key channel 1.0 0))
  (setf (.notes-on self) nil))

(defmethod note-on ((process-data process-data) note sample-offset)
  (note-on (.input-events process-data) note sample-offset)
  (values))

(defmethod swap-in-out ((self process-data))
  (let ((wrap (.wrap self)))
    (psetf (.inputs self)
           (.outputs self)
           (sb:vst-process-data.inputs wrap)
           (sb:vst-process-data.outputs wrap)
           (.outputs self)
           (.inputs self)
           (sb:vst-process-data.outputs wrap)
           (sb:vst-process-data.inputs wrap)

           (.input-events self)
           (.output-events self)
           (sb:vst-process-data.input-events wrap)
           (sb:vst-process-data.output-events wrap)
           (.output-events self)
           (.input-events self)
           (sb:vst-process-data.output-events wrap)
           (sb:vst-process-data.input-events wrap))))

(in-package :utaticl.core)

(defmethod initialize-instance :after ((self process-data)
                                       &key (audio-input-bus-count 1)
                                         (audio-output-bus-count 1))
  (setf (.inputs self) (loop repeat audio-input-bus-count
                             collect (make-instance 'audio-bus :nchannels 2)))
  (setf (.outputs self) (loop repeat audio-output-bus-count
                              collect (make-instance 'audio-bus :nchannels 2))))

(defmethod .audio-input-bus-count ((process-data process-data))
  (length (.inputs process-data)))

(defmethod .audio-output-bus-count ((process-data process-data))
  (length (.outputs process-data)))

(defmethod prepare ((self process-data))
  (prepare (.inputs self))
  (prepare (.outputs self))
  (prepare (.input-events self))
  (prepare (.output-events self)))

(defmethod note-off ((process-data process-data) note sample-offset)
  (note-off (.input-events process-data) note sample-offset)
  (values))

;;; TODO これ系は vst3 じゃなくて、ここでよかったかも
(defmethod note-off-all ((self process-data))
  (loop for (key . channel) in (.notes-on self)
        do (note-off self key channel 1.0 0))
  (setf (.notes-on self) nil))

(defmethod note-on ((process-data process-data) note sample-offset)
  (note-on (.input-events process-data) note sample-offset)
  (values))

(Defmethod swap-in-out ((self process-data))
  (psetf (.inputs self)
         (.outputs self)
         (.outputs self)
         (.inputs self)

         (.input-events self)
         (.output-events self)
         (.output-events self)
         (.input-events self)))

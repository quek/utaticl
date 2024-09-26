(in-package :utaticl.core)

(defmethod note-on ((event-buffer event-buffer) note sample-offset)
  (vector-push-extend :on (.events event-buffer))
  (vector-push-extend note (.notes event-buffer))
  (vector-push-extend sample-offset (.sample-offsets event-buffer)))

(in-package :utaticl.core)

(defmethod note-on ((note-buffer note-buffer) note sample-offset)
  (vector-push-extend :on (.events note-buffer))
  (vector-push-extend note (.notes note-buffer))
  (vector-push-extend sample-offset (.sample-offsets note-buffer)))

(in-package :utaticl.core)

(defmethod note-off ((self note-buffer) note sample-offset)
  (vector-push-extend :off (.events self))
  (vector-push-extend note (.notes self))
  (vector-push-extend sample-offset (.sample-offsets self)))

(defmethod note-on ((self note-buffer) note sample-offset)
  (vector-push-extend :on (.events self))
  (vector-push-extend note (.notes self))
  (vector-push-extend sample-offset (.sample-offsets self)))

(defmethod prepare ((self note-buffer))
  (setf (fill-pointer (.events self)) 0)
  (setf (fill-pointer (.notes self)) 0)
  (setf (fill-pointer (.sample-offsets self)) 0))

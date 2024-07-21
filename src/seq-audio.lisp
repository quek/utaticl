(in-package :dgw)

(defmethod initialize-instance :after ((seq-audio seq-audio) &key path)
  (when path
   (setf (.path seq-audio) path)))

(defmethod (setf .path) :after (path (seq-audio seq-audio))
  (let ((riff (wav:read-wav-file path)))
    (loop for chunk in riff
          if (equal "fmt " (getf chunk :chunk-id))
            do (let ((data (getf chunk :chunk-data)))
                 (setf (.nchannels seq-audio) (getf data :number-of-channels))
                 (setf (.sample-rate seq-audio) (getf data :sample-rate)))
          if (equal "data" (getf chunk :chunk-id))
            do (setf (.data seq-audio) (getf chunk :chunk-data)))))

(defmethod update-duration ((seq-audio seq-audio) bpm)
  (let* ((nframes (/ (length (.data seq-audio)) (.nchannels seq-audio))))
    (/ nframes (.sample-rate seq-audio) (/ 60.0 bpm))))

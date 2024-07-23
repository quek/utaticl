(in-package :dgw)

(defmethod initialize-instance :after ((seq-audio seq-audio) &key name path)
  (when path
    (setf (.path seq-audio) path)
    (unless name
      (setf (.name seq-audio) (pathname-name path)))))

(defmethod (setf .path) :after (path (seq-audio seq-audio))
  (when path
   (let ((riff (wav:read-wav-file path)))
     (loop for chunk in riff
           if (equal "fmt " (getf chunk :chunk-id))
             do (let ((data (getf chunk :chunk-data)))
                  (setf (.nchannels seq-audio) (getf data :number-of-channels))
                  (setf (.sample-rate seq-audio) (getf data :sample-rate)))
           if (equal "data" (getf chunk :chunk-id))
             do (let* ((chunk-data (getf chunk :chunk-data))
                       (size-of-float (cffi:foreign-type-size :float))
                       (length (/ (length chunk-data) size-of-float))
                       (buffer (make-array length
                                           :element-type 'single-float)))
                  (loop for i below length
                        do (setf (aref buffer i)
                                 (coerce (logior (ash (aref chunk-data (* i size-of-float)) 24)
                                                 (ash (aref chunk-data (+ (* i size-of-float) 1)) 16)
                                                 (ash (aref chunk-data (+ (* i size-of-float) 2)) 8)
                                                 (+ (* i size-of-float) 3))
                                         'single-float)))
                  (setf (.data seq-audio) buffer))))))

(defmethod prepare-event ((seq-audio seq-audio) start end loop-p offset-samples)
  (loop with bus = 0
        with nchannels = (.nchannels seq-audio)
        with data = (.data seq-audio)
        for channel below (min nchannels 2)
        for buffer = (buffer (.outputs *process-data*) bus channel)
        do (loop for i below (.frames-per-buffer *config*)
                 do (setf (cffi:mem-aref buffer :float i)
                          (aref data (+ (* i nchannels) channel))))))

(defmethod update-duration ((seq-audio seq-audio) bpm)
  (let* ((nframes (/ (length (.data seq-audio)) (.nchannels seq-audio))))
    (setf (.duration seq-audio) (/ nframes (.sample-rate seq-audio) (/ 60.0 bpm)))))

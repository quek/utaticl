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
                   (setf (.sample-rate seq-audio) (getf data :sample-rate))
                   (setf (.bits-per-sample seq-audio) (getf data :significant-bits-per-sample)))
            if (equal "data" (getf chunk :chunk-id))
              do (let* ((chunk-data (getf chunk :chunk-data))
                        (bits-per-sample (.bits-per-sample seq-audio))
                        (positive-max (1- (/ (ash 1 bits-per-sample) 2)))
                        (negative-operand (ash 1 bits-per-sample))
                        (float-operand (1- (/ (ash 1 bits-per-sample) 2.0)))
                        (length-per-sample (/ bits-per-sample 8))
                        (length (/ (length chunk-data) length-per-sample))
                        (buffer (make-array length
                                            :element-type 'single-float)))
                   (loop for i below length
                         do (setf (aref buffer i)
                                  (/ (loop with n = 0
                                           for j below length-per-sample
                                           do (setf (ldb (byte 8 (* 8 j)) n)
                                                    (aref chunk-data (+ (* i length-per-sample) j)))
                                           finally (return (if (> n positive-max)
                                                               (- n negative-operand)
                                                               n)))
                                     float-operand)))
                   (setf (.data seq-audio) buffer))))))

(defmethod prepare-event ((seq-audio seq-audio) start end loop-p offset-samples)
  (loop with bus = 0
        with nchannels = (.nchannels seq-audio)
        with data = (.data seq-audio)
        with frame-rate = (* (/ 60 (.bpm *project*)) (.sample-rate *config*))
        with start-frame = (round (* start frame-rate))
        with end-frame = (round (* end frame-rate))
        with nframes = (min (- end-frame start-frame) (- (/ (length data) nchannels) start-frame))
        for channel below (min nchannels 2)
        for buffer = (buffer (.outputs *process-data*) bus channel)
        do (assert (<= nframes (.frames-per-buffer *config*)))
        do (loop for i below nframes
                 do (setf (cffi:mem-aref buffer :float i)
                          (aref data (+ (* (+ i start-frame) nchannels) channel))))))

(defmethod update-duration ((seq-audio seq-audio) bpm)
  (let* ((nframes (/ (length (.data seq-audio)) (.nchannels seq-audio))))
    (setf (.duration seq-audio) (/ nframes (.sample-rate seq-audio) (/ 60.0 bpm)))))

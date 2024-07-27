(in-package :dgw)

(defmethod initialize-instance :after ((seq-audio seq-audio) &key name path)
  (when path
    (setf (.path seq-audio) path)
    (unless name
      (setf (.name seq-audio) (pathname-name path)))))

(defmethod (setf .path) :after (path (seq-audio seq-audio))
  (when path
    (read-wav seq-audio)))

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

(defmethod read-wav ((seq-audio seq-audio))
  (loop with riff = (wav:read-wav-file (.path seq-audio))
        with compression-code = 0
        for chunk in riff
        if (equal "fmt " (getf chunk :chunk-id))
          do (let ((data (getf chunk :chunk-data)))
               (setf (.nchannels seq-audio) (getf data :number-of-channels))
               (setf (.sample-rate seq-audio) (getf data :sample-rate))
               (setf (.bits-per-sample seq-audio) (getf data :significant-bits-per-sample))
               (setf compression-code (getf data :compression-code)))
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
                              (cond ((= compression-code 1)
                                     (loop with n = 0
                                           for j below length-per-sample
                                           do (setf (ldb (byte 8 (* 8 j)) n)
                                                    (aref chunk-data (+ (* i length-per-sample) j)))
                                           finally (return (/ (if (> n positive-max)
                                                                  (- n negative-operand)
                                                                  n)
                                                              float-operand))))
                                    ((= compression-code 3)
                                     (cffi:mem-aref (sb-sys:vector-sap chunk-data) :float
                                                    i)))))
               (setf (.data seq-audio)
                     (if (= (.sample-rate seq-audio) (.sample-rate *config*))
                         buffer
                         (prog1
                             (src-ffi::simple buffer
                                              (.sample-rate seq-audio)
                                              (.sample-rate *config*)
                                              (.nchannels seq-audio))
                           (setf (.sample-rate seq-audio) (.sample-rate *config*))))))))

(defmethod update-duration ((seq-audio seq-audio) bpm)
  (let* ((nframes (/ (length (.data seq-audio)) (.nchannels seq-audio))))
    (setf (.duration seq-audio) (/ nframes (.sample-rate seq-audio) (/ 60.0 bpm)))))

(defmethod render-in-arrangement ((seq-audio seq-audio) pos1 pos2 pos1-visible pos2-visible)
  (let ((width (- (.x pos2) (.x pos1)))
        (height (round (- (.y pos2) (.y pos1))))
        (width-cache (car (.waveform-cache seq-audio)))
        (height-cache (cadr (.waveform-cache seq-audio)))
        (waveform (caddr (.waveform-cache seq-audio)))
        (draw-list (ig:get-window-draw-list)))
    (when (or (null width-cache) (null height-cache)
              (/= width width-cache)
              (/= height height-cache))
      (setf (.waveform-cache seq-audio)
            (list width height
                  (loop with nchannels = (.nchannels seq-audio)
                        with nframes = (/ (length (.data seq-audio)) nchannels)
                        with frames-per-pixcel = (/ nframes height)
                        with data = (.data seq-audio)
                        for i below height
                        if (< (.y pos2-visible) (+ (.y pos1) i))
                          do (loop-finish)
                        if (<= (.y pos1-visible) (+ (.y pos1) i))
                          collect (multiple-value-list
                                   (if (< frames-per-pixcel 1)
                                       (progn
                                         (values .5 (random .99)))
                                       (let* ((start (round (* frames-per-pixcel i)))
                                              (end (min (+ start (round frames-per-pixcel)) nframes)))
                                         (loop for j from start below end
                                               for value = (aref data (* j nchannels))
                                               minimize value into min
                                               maximize value into max
                                               finally (return (values min max))))))))))
    (loop for i below height
          for (min max) in waveform
          with width-half = (float (/ width 2))
          for p1 = (@+ pos1 (@ (+ (* width-half min) width-half) (float i)))
          for p2 = (@+ pos1 (@ (+ (* width-half max) width-half) (float i)))
          if (< (.y pos2-visible) (.y p1))
            do (loop-finish)
          if (<= (.y pos1-visible) (.y p1))
            do (ig:add-line draw-list p1 p2 (color #xff #xff #xff #x80)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :wav)

(defun format-chunk-data-reader (stream chunk-id chunk-data-size)
  "Reads and parses the chunk-data from a format chunk."
  (declare (ignore chunk-id chunk-data-size))
  (let ((compression-code (riff:read-u2 stream))
        (number-of-channels (riff:read-u2 stream))
        (sample-rate (riff:read-u4 stream))
        (average-bytes-per-second (riff:read-u4 stream))
        (block-align (riff:read-u2 stream))
        (significant-bits-per-sample (riff:read-u2 stream)))

    (if (or (eql compression-code 1) (eql compression-code 3)) ;3の場合を追加
        (list :compression-code compression-code
              :number-of-channels number-of-channels
              :sample-rate sample-rate
              :average-bytes-per-second average-bytes-per-second
              :block-align block-align
              :significant-bits-per-sample significant-bits-per-sample)

        (let*
          ((extra-format-bytes (riff:read-u2 stream))
           (buffer (make-array extra-format-bytes :element-type (stream-element-type stream)))
           (extra-bytes (read-sequence buffer stream)))
          (declare (ignore extra-bytes))
          (list :compression-code compression-code
                :number-of-channels number-of-channels
                :sample-rate sample-rate
                :average-bytes-per-second average-bytes-per-second
                :block-align block-align
                :significant-bits-per-sample significant-bits-per-sample
                :extra-format-bytes extra-format-bytes
                :bytes buffer)))))

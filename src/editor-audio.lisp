(in-package :utaticl.core)

(defmethod render-body ((self editor-audio))
  (let* ((width (- (.x *window-size*) (.offset-x self) *scrollbar-size*))
         (height (- (.y *window-size*) (.offset-y self))))
    (when (plusp height)
      (loop for sample in (.samples (.seq (.clip self)))
            do (%editor-audio-render-sample self sample width height))
      (zoom-y-update self (ig:get-io)))))

(defmethod render-header ((self editor-audio))
  )

(defun %editor-audio-render-sample (self sample width height)
  ;;(declare (optimize (speed 3)))
  (let* ((nchannels (.nchannels sample))
         (nframes (/ (length (.data sample)) nchannels))
         (sample-height (floor (* (.duration sample) *zoom-y*)))
         (frames-per-pixcel (/ nframes sample-height))
         (i-end (min height sample-height))
         (data (.data sample))
         (xs (loop for i from 0 below i-end
                   for last-y = 0.0 then y
                   for y = (+ (.y *window-pos*) i)
                   for data-start = (round (+ (* frames-per-pixcel (+ i *scroll-y*))))
                   for data-end = (min (+ data-start (ceiling frames-per-pixcel)) nframes)
                   with last-x = most-negative-short-float
                   with last-skipped-p = nil
                   nconc (let ((min most-positive-short-float)
                               (max most-negative-short-float))
                           (loop for data-index from data-start below data-end
                                 for value = (aref data (* data-index nchannels))
                                 for x = (+ (.x *window-pos*)
                                            (.offset-x self)
                                            (* width (/ (+ value 1.0) 2.0)))
                                 do (setf min (min min x))
                                    (setf max (max max x)))
                           (if (<= frames-per-pixcel 1)
                               (if (and (/= i (1- i-end)) (= last-x min))
                                   (progn
                                     (setf last-skipped-p t)
                                     ())
                                   (progn
                                     (if last-skipped-p
                                         (prog1 (list (cons last-y last-x) (cons y min))
                                           (setf last-skipped-p nil)
                                           (setf last-x min))
                                         (progn
                                           (setf last-x min)
                                           (list (cons y min))))))
                               (list (cons y min)
                                     (cons y max)))))))
    (autowrap:with-alloc (p 'ig:im-vec2 (length xs))
      (loop for (y . x) in xs
            for i from 0
            do (setf (plus-c:c-ref p ig:im-vec2 i :x) x)
               (setf (plus-c:c-ref p ig:im-vec2 i :y) y))
      (ig:im-draw-list-add-polyline *draw-list* p (length xs) (color #x00 #xff #xff)
                                    0 1.0))
    ;; 以下デバッグ
    (ig:set-cursor-pos (@ 40.0 (+ 40.0 *scroll-y*)))
    (ig:with-group
      (ig:text (format nil "*mouse-pos* ~a" *mouse-pos*))
      (ig:text (format nil "height ~a" height))
      (ig:text (format nil "nframes ~a" nframes))
      (ig:text (format nil "frames-per-pixcel ~a ~a" frames-per-pixcel (float frames-per-pixcel)))
      (ig:text (format nil "i-end ~a" i-end))
      (ig:text (format nil "sample duration ~a" (.duration sample)))
      (ig:text (format nil "sample-height ~a" sample-height))
      (ig:text (format nil "*zoom-y* ~a" *zoom-y*))
      (ig:text (format nil "(length xs) ~a" (length xs)))
      (ig:text (format nil "(car xs) ~a (cadr xs) ~a (last xs) ~a" (car xs) (cadr xs) (last xs)))
      (ig:text (format nil "~a" xs))
      ))
  #+nil
  (let ((xs (loop with nchannels = (.nchannels sample)
                  with nframes = (/ (length (.data sample)) nchannels)
                  with frames-per-pixcel = (/ nframes height)
                  with data = (.data sample)
                  for i from 0 below height
                  collect (if (< frames-per-pixcel 1)
                              (let* ((j (floor (* i (/ nframes height))))
                                     (value (aref data (* j nchannels))))
                                (list value value))
                              (let* ((j-start (round (* frames-per-pixcel i)))
                                     (j-end (min (+ j-start (round frames-per-pixcel)) nframes)))
                                (loop for j from j-start below j-end
                                      for value = (aref data (* j nchannels))
                                      minimize value into min
                                      maximize value into max
                                      finally (return (list min max))))))))
    (autowrap:with-alloc (p 'ig:im-vec2 height)
      (loop for (min max) in xs
            for i from 0
            for pos = (@+ *window-pos*
                          (@ (* width (/ (+ max 1.0) 2.0)) i)
                          (@ (.offset-x self) 0.0)
                          (@ 0.0 (- *scroll-y*)))
            do (setf (plus-c:c-ref p ig:im-vec2 i :x) (.x pos))
               (setf (plus-c:c-ref p ig:im-vec2 i :y) (.y pos)))
      (ig:im-draw-list-add-polyline *draw-list* p height (color #xff #xff #xff)
                                    0 1.0))
    (autowrap:with-alloc (p 'ig:im-vec2 height)
      (loop for (min max) in xs
            for i from 0
            for pos = (@+ *window-pos*
                          (@ (* width (/ (+ min 1.0) 2.0)) i)
                          (@ (.offset-x self) 0.0)
                          (@ 0.0 (- *scroll-y*)))
            do (setf (plus-c:c-ref p ig:im-vec2 i :x) (.x pos))
               (setf (plus-c:c-ref p ig:im-vec2 i :y) (.y pos)))
      (ig:im-draw-list-add-polyline *draw-list* p height (color #xff #xff #xff)
                                    0 1.0))))

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
  ;; (declare (optimize (speed 3) (safety 0)))
  (let* ((nchannels (.nchannels sample))
         (nframes (/ (length (.data sample)) nchannels))
         (sample-height (floor (* (.duration sample) *zoom-y*)))
         (frames-per-pixcel (/ nframes sample-height))
         (i-end (min height (- sample-height *scroll-y*)))
         (data (.data sample))
         (xs (loop for i from 0 below i-end
                   for y = (+ (.y *window-pos*) i)
                   for data-start = (round (+ (* frames-per-pixcel (+ i *scroll-y*))))
                   for data-end = (min (+ data-start (ceiling frames-per-pixcel)) nframes)
                   with last-x = most-negative-short-float
                   with last-skipped-p = nil
                   with last-min = (+ (.x *window-pos*)
                                      (.offset-x self)
                                      (round (/ width 2.0)))
                   with last-max = last-min
                   nconc (let ((min most-positive-short-float)
                               (max most-negative-short-float))
                           (loop for data-index from data-start below data-end
                                 for value = (aref data (* data-index nchannels))
                                 for x = (+ (.x *window-pos*)
                                            (.offset-x self)
                                            (round (* width (/ (+ value 1.0) 2.0))))
                                 do (setf min (min min x))
                                    (setf max (max max x)))
                           (if (<= frames-per-pixcel 1)
                               (if (and (/= i (1- i-end)) (= last-x min))
                                   (progn
                                     (setf last-skipped-p t)
                                     ())
                                   (progn
                                     (if last-skipped-p
                                         (prog1 (list (cons y last-x) (cons y min))
                                           (setf last-skipped-p nil)
                                           (setf last-x min))
                                         (progn
                                           (setf last-x min)
                                           (list (cons y min))))))
                               (if (oddp i)
                                   (prog1 (list (cons y last-min)
                                                (cons y (max max last-max)))
                                     (setf last-min min)
                                     (setf last-max (max max last-max)))
                                   (prog1 (list (cons y last-max)
                                                (cons y (min min last-min)))
                                     (setf last-min (min min last-min))
                                     (setf last-max max))))))))
    (autowrap:with-alloc (p 'ig:im-vec2 (length xs))
      (loop for (y . x) in xs
            for i from 0
            do (setf (plus-c:c-ref p ig:im-vec2 i :x) x)
               (setf (plus-c:c-ref p ig:im-vec2 i :y) y))
      (ig:im-draw-list-add-polyline *draw-list* p (length xs) (color #x00 #xff #xff)
                                    0 1.0))
    ;; 以下デバッグ
    ;; (let ((xs (list (cons 250.0 1000.0)
    ;;                 (cons 251.0 1000.0) (cons 251.0 1400.0)
    ;;                 (cons 252.0 1400.0) (cons 252.0 900.0)
    ;;                 (cons 253.0 900.0) (cons 253.0 1500.0)
    ;;                 (cons 254.0 1500.0) (cons 254.0 900.0)
    ;;                 (cons 255.0 900.0) (cons 255.0 1500.0)
    ;;                 (cons 256.0 1500.0) (cons 256.0 900.0)
    ;;                 (cons 257.0 900.0) (cons 257.0 1500.0)
    ;;                 (cons 258.0 1500.0) (cons 258.0 1100.0)
    ;;                 (cons 259.0 1100.0) (cons 259.0 1200.0)
    ;;                 )))

    ;;   (loop for ((y1 . x1) (y2 . x2)) on xs by #'cdr
    ;;         while y2
    ;;         do (ig:add-line *draw-list* (@ x1 y1) (@ x2 y2) (color #x00 #xff #xff)))

    ;;   (autowrap:with-alloc (p 'ig:im-vec2 (length xs))
    ;;     (loop for (y . x) in xs
    ;;           for i from 0
    ;;           do (setf (plus-c:c-ref p ig:im-vec2 i :x) x)
    ;;              (setf (plus-c:c-ref p ig:im-vec2 i :y) (+ 50 y)))
    ;;     (ig:im-draw-list-add-polyline *draw-list* p (length xs) (color #x00 #xff #xff)
    ;;                                   0 1.0)))
    ;; (ig:set-cursor-pos (@ 40.0 (+ 40.0 *scroll-y*)))
    ;; (ig:with-group
    ;;   (ig:text (format nil "*mouse-pos* ~a" *mouse-pos*))
    ;;   (ig:text (format nil "height ~a" height))
    ;;   (ig:text (format nil "nframes ~a" nframes))
    ;;   (ig:text (format nil "frames-per-pixcel ~a ~a" frames-per-pixcel (float frames-per-pixcel)))
    ;;   (ig:text (format nil "i-end ~a" i-end))
    ;;   (ig:text (format nil "sample duration ~a" (.duration sample)))
    ;;   (ig:text (format nil "sample-height ~a" sample-height))
    ;;   (ig:text (format nil "*zoom-y* ~a" *zoom-y*))
    ;;   (ig:text (format nil "(length xs) ~a" (length xs)))
    ;;   (ig:text (format nil "(car xs) ~a (cadr xs) ~a (last xs) ~a" (car xs) (cadr xs) (last xs)))
    ;;   (ig:text (format nil "~a" xs))
    ;;   )
    ))

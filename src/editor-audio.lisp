(in-package :utaticl.core)

(defmethod render-body ((self editor-audio))
  (let* ((width (- (.x *window-size*) (.offset-x self) *scrollbar-size*))
         (height (floor (min (* (.duration (.clip self)) *zoom-y*)
                             (- (.y *window-size*) (.offset-y self)))))
         (io (ig:get-io)))
    (when (plusp height)
      (loop for sample in (.samples (.seq (.clip self)))
            do (%editor-audio-render-sample self sample width height))
      (zoom-y-update self io))))

(defmethod render-header ((self editor-audio))
  )


(defun %editor-audio-render-sample (self sample width height)
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
                          (@ (.offset-x self) 0.0))
            do (setf (plus-c:c-ref p ig:im-vec2 i :x) (.x pos))
               (setf (plus-c:c-ref p ig:im-vec2 i :y) (.y pos)))
      (ig:im-draw-list-add-polyline *draw-list* p height (color #xff #xff #xff)
                                    0 1.0))
    (autowrap:with-alloc (p 'ig:im-vec2 height)
      (loop for (min max) in xs
            for i from 0
            for pos = (@+ *window-pos*
                          (@ (* width (/ (+ min 1.0) 2.0)) i)
                          (@ (.offset-x self) 0.0))
            do (setf (plus-c:c-ref p ig:im-vec2 i :x) (.x pos))
               (setf (plus-c:c-ref p ig:im-vec2 i :y) (.y pos)))
      (ig:im-draw-list-add-polyline *draw-list* p height (color #xff #xff #xff)
                                    0 1.0))))

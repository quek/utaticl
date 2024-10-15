(in-package :utaticl.core)

(defparameter *editor-automation-point-radius* 4.0)

(defmethod render-body ((self editor-automation))
  (loop for point in (.points (.seq (.clip self)))
        do (%editor-automation-render-point self point))

  (when (in-p *mouse-pos* *rect-body*)
    (%editor-automation-body-handle self)))

(defmethod render-header ((self editor-automation))
  (let* ((clip (.clip self))
         (lane (.lane clip))
         (param (.automation-param lane))
         (text-min (value-text param 0d0))
         (text-half (value-text param .5d0))
         (text-max (value-text param 1d0))
         (text-half-size (ig:calc-text-size text-half))
         (text-max-size (ig:calc-text-size text-max))
         (pos-min (@ (+ (.offset-x self) 4.0) .0))
         (scrollbar-size (plus-c:c-ref (ig:get-style) ig:im-gui-style :scrollbar-size))
         (pos-half (@ (+ (.offset-x self)
                         (/ (- (ig:get-window-width)
                               (.offset-x self)
                               scrollbar-size)
                            2)
                         (- (/ (.x text-half-size) 2))
                         -4.0)
                      .0))
         (pos-max (@ (- (ig:get-window-width)
                        scrollbar-size
                        (.x text-max-size)
                        4.0)
                     .0)))
    (ig:set-cursor-pos pos-min)
    (ig:text text-min)
    (ig:set-cursor-pos pos-half)
    (ig:text text-half)
    (ig:set-cursor-pos pos-max)
    (ig:text text-max)))

(defmethod value-to-world-x ((self editor-automation) value)
  (+ (.x *window-pos*)
     (* (.x *window-size*) value)))

(defun %editor-automation-body-handle (self)
  self)

(defun %editor-automation-render-point (self point)
  (let* ((x (value-to-world-x self (.value point)))
         (y (time-to-world-y self (.time point)))
         (center (@ x y)))
    (ig:path-arc-to-fast *draw-list*
                         center
                         *editor-automation-point-radius* 0 12)
    (ig:path-stroke *draw-list* (color #xcc #xcc #xcc #xff) :thickness 2.0))
  #|
  (Ig:path-arc-to-fast *draw-list*
  (@+ center (@ 200.0 .0))
  *editor-automation-point-radius* 0 12)
  (ig:path-stroke *draw-list* (color #xcc #xcc #xcc #xff) :thickness 2.0)
  (ig:path-arc-to-fast *draw-list*
  (@+ center (@ 300.0 100.0))
  *editor-automation-point-radius* -6 6)
  (ig:path-stroke *draw-list* (color #xcc #xcc #xcc #xff) :thickness 2.0)
  |#
  )

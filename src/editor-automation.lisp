(in-package :utaticl.core)

(defparameter *editor-automation-point-radius* 4.0)

(defmethod .points ((self editor-automation))
  (.points (.clip self)))

(defmethod render-body ((self editor-automation))
  (loop for point in (.points (.seq (.clip self)))
        do (%editor-automation-render-point self point))
  (%editor-automation-render-point-area self)
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
     (.offset-x self)
     (* (.width *rect-body*) value)))

(defmethod world-x-to-value ((self editor-automation) x)
  (/ (- x (.x *window-pos*) (.offset-x self))
     (.width *rect-body*)))

(defun %editor-automation-body-handle (self)
  (let ((value (world-x-to-value self (.x *mouse-pos*)))
        (time (world-y-to-time self (.y *mouse-pos*))))
    #+nil
    (progn
      (ig:set-cursor-pos (@ 100.0 100.0))
      (ig:text (format nil "~a $ ~a" time value)))
    (cond ((and (null (.item-at-mouse self)))
           (cond ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
                  (cmd-add *project* 'cmd-automation-point-add
                           :seq (.seq (.clip self))
                           :time time
                           :value value)))))))

(defun %editor-automation-render-point (self point)
  (let* ((x (value-to-world-x self (.value point)))
         (y (time-to-world-y self (.time point)))
         (center (@ x y))
         (color (color-selected (.color-automation-point *theme*)
                                (member point (.items-selected self)))))
    (ig:path-arc-to-fast *draw-list*
                         center
                         *editor-automation-point-radius* 0 12)
    (ig:path-stroke *draw-list* color :thickness 2.0)
    (when (%editor-automation-point-at-mouse-p center)
      (setf (.item-at-mouse self) point)
      (when (ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
        (if (key-ctrl-p)
            (setf (.items-selected self)
                  (if (member point (.items-selected self))
                      (remove point (.items-selected self))
                      (cons point (.items-selected self))))
            (setf (.items-selected self)
                  (list point)))))))

(defun %editor-automation-render-point-area (self)
  (when (.points self)
    (loop for point in (.points self)
          for x = (value-to-world-x self (.value point))
          for y = (time-to-world-y self (.time point))
          for center = (@ x y)
          for first-p = t then nil
          if first-p
            do (ig:path-line-to *draw-list*
                                (@ (value-to-world-x self .0)
                                   (time-to-world-y self .0)))
               (ig:path-line-to *draw-list*
                                (@ x    ;initially だと x が nil になる
                                   (time-to-world-y self .0)))
          do (ig:path-line-to *draw-list* center)
          finally (ig:path-line-to *draw-list*
                                   (@ x
                                      (time-to-world-y self (.duration (.clip self)))))
                  (ig:path-line-to *draw-list*
                                   (@ (value-to-world-x self .0)
                                      (time-to-world-y self (.duration (.clip self))))))
    (ig:path-fill-concave *draw-list* (.color-automation-fill *theme*))))

(defun %editor-automation-point-at-mouse-p (center)
  (and (<= (- (.x center) *editor-automation-point-radius*)
           (.x *mouse-pos*)
           (+ (.x center) *editor-automation-point-radius*))
       (<= (- (.y center) *editor-automation-point-radius*)
           (.y *mouse-pos*)
           (+ (.y center) *editor-automation-point-radius*))))

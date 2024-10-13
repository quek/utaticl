(in-package :utaticl.core)

(defmethod handle-shortcut ((self editor)))

(defmethod render ((self editor))
  (ig:with-begin ((.name self) :flags ig:+im-gui-window-flags-no-scrollbar+)
    (render-grid self)
    (ig:text (.name (.clip self)))
    (ig:with-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)

      (render-time-ruler self)

      (let* ((window-pos (ig:get-window-pos))
             (window-size (ig:get-window-size))
             (clip-rect-min (@+ window-pos (@ (.offset-x self) .0)))
             (clip-rect-max (@- (@+ window-pos window-size)
                                (@ (plus-c:c-ref (ig:get-style) ig:im-gui-style :scrollbar-size)
                                   .0))))
        (ig:with-clip-rect (clip-rect-min clip-rect-max)
          (render-header self))
        (ig:with-clip-rect ((@+ window-pos (@ (.offset-x self) (.offset-y self)))
                            (@+ window-pos window-size))
          (render-body self)))

      (swhen (.render-first-p self)
        (setf it (view-fit self))))

    (handle-shortcut self)))

(defgeneric render-header (editor))
(defgeneric render-body (editor))

(defmethod view-fit ((self editor))
  nil)

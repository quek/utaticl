(in-package :dgw)

(defmethod handle-mouse ((self piano-roll))
  (let* ((io (ig:get-io))
         (mouse-pos (ig:get-mouse-pos)))
    (cond ((ig:is-mouse-double-clicked ig:+im-gui-mouse-button-left+)
           (multiple-value-bind (time key) (world-pos-to-time-key self mouse-pos)
             (when (and (not (minusp time)) key)
               (cmd-add *project* 'cmd-note-add
                        :clip-id (.neko-id (.clip self)) :time time :key key)))))
    (zoom-x-update self io)
    (zoom-y-update self io)))

(defmethod render ((self piano-roll))
  (when (ig:begin "##piano-roll" :flags ig:+im-gui-window-flags-no-scrollbar+)
    (ig:text (.name (.clip self)))
    (when (ig:begin-child "##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
      (render-time-ruler self)
      (render-keyboard self)
      (handle-mouse self))
    (ig:end-child))
  (ig:end))

(defmethod render-keyboard ((self piano-roll))
  (loop for key from +g9+ downto +c-1+
        do (ig:text (midi-key-name key))))

(defmethod world-pos-to-time-key ((self piano-roll) pos)
  (let* ((time (world-x-to-time self (.x pos)))
         (key (world-y-to-key self (.y pos))))
    (values time key)))

(defmethod world-x-to-time ((self piano-roll) x)
  (+ (/ (- x (.x (ig:get-window-pos)) (.offset-x self))
        (.zoom-x self))
     (ig:get-scroll-x)))

(defmethod world-y-to-key ((self piano-roll) y)
  (let ((local-y (+ (- y (.y (ig:get-window-pos)) (.time-ruler-height self))
                    (ig:get-scroll-y))))
    (floor (/ local-y (.zoom-y self)))))

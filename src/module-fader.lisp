(in-package :utaticl.core)

(defmethod initialize-instance :after ((self module-fader) &key)
  (param-add self (make-instance 'param :id 'mute :name "Mute" :value .0d0))
  (param-add self (make-instance 'param :id 'pan :name "Pan" :value .5d0))
  (param-add self (make-instance 'param :id 'solo :name "Solo" :value .0d0))
  (param-add self (make-instance 'param :id 'volume :name "Volume" :value 0.8d0)))

(defmethod process :before ((self module-fader))
  (process-before (.volume-meter self)))

(defmethod process :after ((self module-fader))
  (process-after (.volume-meter self)))

(defmethod process-sample ((self module-fader) sample0 sample1)
  (let* ((db (%module-fader-db self))
         (ratio (from-db db))
         (value0 (coerce (* sample0
                            ratio
                            (* (- 1.0 (.value (param self 'pan)))
                               2.0))
                         'single-float))
         (value1 (coerce (* sample1
                            ratio
                            (* (.value (param self 'pan))
                               2.0))
                         'single-float)))
    (change (.volume-meter self) value0 value1)
    (values value0 value1)))

(defmethod render-in ((self module-fader) (rack rack) &key)
  (ig:with-group
    (render-in (.volume-meter self) rack :fader self))
  (ig:same-line)
  (ig:v-slider-scalar "##Vol"
                      (@ 22.0 (- (.y *window-size*) *scrollbar-size*
                                 *item-spacing-y*))
                      ig:+im-gui-data-type-double+
                      (.value (param self 'volume))
                      0.0d0 1.0d0
                      :format (format nil "~,2f~%~,2f" (%module-fader-db self) (.value (param self 'volume))))
  (when (and (ig:is-item-hovered)
             (ig:is-mouse-clicked ig:+im-gui-mouse-button-right+))
    (setf (.value (param self 'volume)) 0.8d0))
  (ig:same-line)
  (ig:v-slider-scalar "##Pan"
                      (@ 22.0 (- (.y *window-size*) *scrollbar-size*
                                 *item-spacing-y*))
                      ig:+im-gui-data-type-double+
                      (.value (param self 'pan))
                      0.0d0
                      1.0d0))


(defun %module-fader-db (self)
  (let* ((db (expt (.value (param self 'volume))
                   (/ 1 *meter-expt*)))
         (db (* db (- *meter-max-db* *meter-min-db*)))
         (db (+ db *meter-min-db*)))
    db))

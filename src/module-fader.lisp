(in-package :utaticl.core)

(defmethod initialize-instance :after ((self module-fader) &key)
  (param-add self (make-instance 'param :id 'mute :name "Mute" :value .0d0))
  (param-add self (make-instance 'param :id 'pan :name "Pan" :value .5d0))
  (param-add self (make-instance 'param :id 'solo :name "Solo" :value .0d0))
  (param-add self (make-instance 'param :id 'volume :name "Volume" :value 1.0d0)))

(defmethod process-sample ((self module-fader) sample0 sample1)
  (let ((value0 (coerce (* sample0
                           (.value (param self 'volume))
                           (* (- 1.0 (.value (param self 'pan)))
                              2.0))
                        'single-float))
        (value1 (coerce (* sample1
                           (.value (param self 'volume))
                           (* (.value (param self 'pan))
                              2.0))
                        'single-float)))
    (change (.peak-meter self) value0 value1)
    (values value0 value1)))

(defmethod render ((self module-fader))
  (render (.peak-meter self))
  (ig:drag-scalar "Vol"
                  ig:+im-gui-data-type-double+
                  (.value (param self 'volume))
                  :speed .01
                  :min .0d0
                  :max 1.0d0
                  :format "%.2f")
  (ig:drag-scalar "Pan"
                  ig:+im-gui-data-type-double+
                  (.value (param self 'pan))
                  :speed .01
                  :min .0d0
                  :max 1.0d0
                  :format "%.2f"))


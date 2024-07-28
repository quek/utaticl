(in-package :dgw)

(defmethod initialize-instance :after ((self module-fader) &key)
  (param-add self 'mute "Mute" .0d0)
  (param-add self 'pan "Pan" .5d0)
  (param-add self 'solo "Solo" .0d0)
  (param-add self 'volume "Volume" 1.0d0))

(defmethod render ((module-fader module-fader))
  (ig:drag-scalar "VOL"
                  ig:+im-gui-data-type-double+
                  (.value (param module-fader 'volume))
                  :speed .01
                  :min .0d0
                  :max 1.0d0
                  :format "%.2f"))

(defmethod process-sample ((self module-fader) sample)
  ;; TODO
  (coerce (* sample (.value (param self 'volume)))
          'single-float))


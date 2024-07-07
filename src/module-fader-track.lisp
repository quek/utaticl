(in-package :dgw)

(defmethod render ((module-fader-track module-fader-track))
  (ig:drag-scalar "VOL"
                  ig:+im-gui-data-type-double+
                  (.value (param module-fader-track 'volume))
                  :speed .01
                  :min .0d0
                  :max 1.0d0
                  :format "%.2f"))

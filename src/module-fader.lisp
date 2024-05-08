(in-package :dgw)

(defmethod initialize-instance :after ((self module-fader) &key)
  (param-add self 'mute "Mute" .0d0)
  (param-add self 'pan "Pan" .5d0)
  (param-add self 'solo "Solo" .0d0)
  (param-add self 'volume "Volume" .8d0))

(defmethod process-sample ((self module-fader) sample)
  ;; TODO
  (coerce (* sample (.value (param self 'volume)))
          'single-float))


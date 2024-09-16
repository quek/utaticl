(in-package :utaticl.core)

(defmethod initialize-instance :after ((self module-gain) &key)
  (param-add self (make-instance 'param :id 'volume :name "Volume" :value 1.0d0)))

(defmethod process-sample ((self module-gain) sample)
  (coerce (* sample (.value (param self 'volume)))
          'single-float))


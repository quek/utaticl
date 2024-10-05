(in-package :utaticl.core)

(defmethod param-change-add ((self module-plugin) (param param)
                             &optional (sample-offset 0))
  (sb-concurrency:send-message (.param-changes-mbox-in self)
                               (list (.id param)
                                     (.value param)
                                     sample-offset)))

(in-package :utaticl.core)

(defmethod begin-edit ((self module-plugin) id)
  (let ((param (gethash id (.params self))))
    (begin-edit param id)))

(defmethod end-edit ((self module-plugin) id)
  (let ((param (gethash id (.params self))))
    (end-edit param id)))

(defmethod param-change-add ((self module-plugin) (param param)
                             &optional (sample-offset 0))
  (sb-concurrency:send-message (.param-changes-mbox-in self)
                               (list (.id param)
                                     (.value param)
                                     sample-offset)))

(defmethod perform-edit ((self module-plugin) id value)
  (let ((param (gethash id (.params self))))
    (perform-edit param id value)))

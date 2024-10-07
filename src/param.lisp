(in-package :utaticl.core)

(defmethod automate-p ((self param))
  t)

(defmethod begin-edit ((self param) _)
  (setf (.begin-edit-p self) t)
  (setf (.begin-edit-value self) (.value self))
  (setf (.editing-p self) t))

(defmethod end-edit ((self param) _)
  (setf (.begin-edit-p self) nil)
  (setf (.editing-p self) nil)
  (when (/= (.begin-edit-value self) (.value self))
    (cmd-add (.project self)
             'cmd-param-value
             :param self
             :value-new (.value self)
             :value-old (.begin-edit-value self))))

(defmethod perform-edit ((self param) id value)
  (unless (.begin-edit-p self)
    ;; begin-edit なしで呼ばれる場合もある
    (unless (.editing-p self)
      (setf (.begin-edit-value self) (.value self))
      (setf (.editing-p self) t)
      (params-ordered-update (.module self) self))
    (when (.perform-timer self)
      (sb-ext:unschedule-timer (.perform-timer self)))
    (let ((timer (sb-ext:make-timer
                  (lambda ()
                    (setf (.perform-timer self) nil)
                    ;; TODO cmd-add をスレッドセーフにすべき？
                    (end-edit self nil)))))
      (sb-ext:schedule-timer timer 2)
      (setf (.perform-timer self) timer)))
  (setf (.value self) value))

(defmethod .project ((self param))
  (.project (.module self)))

(defmethod value-changed-by-host ((self param)))

(defmethod value-text ((param param))
  (format nil "~,2f" (.value param)))


(defpackage :utaticl.module
  (:use :cl :utaticl.core))

(in-package :utaticl.module)

(defmethod render-params ((module module))
  (loop for i below 4
        for param in (.params-ordered module)
        do (ig:set-next-item-width 200.0)
           (when (ig:drag-scalar (format nil "~a ~d" (.name param) (.step-count param))
                                 ig:+im-gui-data-type-double+
                                 (.value param)
                                 :speed .005
                                 :min .0d0
                                 :max 1.0d0
                                 :format (format nil "%.2f (~a)" (value-text param)))
             (value-changed-by-host param))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :utaticl.core)

(sb-ext:defglobal *hwnd-module-map* (make-hash-table :weakness :value))

(defmethod connect ((from module) (to module))
  (let ((connection (make-instance 'connection
                                        :from from
                                        :to to
                                        :from-bus-index 0
                                        :to-bus-index 0)))
    (push connection (.connections from))
    (push connection (.connections to))))

(defmethod connections-from ((self module))
  (loop for connection in (.connections self)
        if (eq (.to connection) self)
          collect connection))

(defmethod connections-to ((self module))
  (loop for connection in (.connections self)
        if (eq (.from connection) self)
          collect connection))

(defmethod deserialize-after ((self module))
  (initialize self)
  (start self))

(defmethod deserialize-slot ((self module) (slot (eql 'state)) value)
  (setf (state self) value))


(defmethod disconnect ((from module) (to module))
  "from is a fader, to is a gain"
  (flet ((pred (connection)
           (and (eq (.from connection) from)
                (eq (.to connection) to))))
    (setf (.connections from)
          (delete-if #'pred (.connections from)))
    (setf (.connections to)
          (delete-if #'pred (.connections to)))))

(defmethod editor-open :around ((module module))
  (unless (.editor-open-p module)
    (when (call-next-method)
      (setf (.editor-open-p module) t))))

(defmethod editor-open ((module module)))

(defmethod editor-close :around ((self module))
  (when (.editor-open-p self)
    (when (call-next-method)
      (setf (.editor-open-p self) nil))))

(defmethod editor-close ((module module)))

(defmethod initialize ((self module)))

(defmethod (setf .latency-pdc) :after (value (self module))
  (loop for connection in (.connections self)
        if (eq self (.to connection))
          do (setf (.latency-pdc connection)
                   (if (< (.latency-pdc (.from connection)) value)
                       (- value (.latency-pdc (.from connection)))
                       0))))

(defmethod param-add ((module module) (param param))
  (setf (gethash (.id param) (.params module)) param)
  (setf (.params-ordered module)
        (append (.params-ordered module) (list param)))
  (setf (.module param) module))

(defmethod param-editing ((module module) id value)
  (param-editing module (gethash id (.params module)) value))

(defmethod param-editing ((module module) (param param) value)
  (setf (.value param) value))

(defmethod params-clear ((module module))
  (clrhash (.params module))
  ;; TODO 順番は保持したいからクリアすべきではない？
  (setf (.params-ordered module) ()))

(defmethod params-prepare ((module module))
  )

(defmethod prepare ((self module))
  (setf (.process-done self) nil))

(defmethod process-connection ((self module))
  (loop for connection-from in (connections-from self)
        do (process connection-from)))

(defmethod process ((self module))
  (loop for connection in (connections-to self)
        do (setf (.from-process-data connection) *process-data*))
  (setf (.process-done self) t))

(defmethod .project ((self module))
  (.project (.track self)))

(defmethod render ((module module))
  (ig:with-id (module)
    (when (ig:button (.name module))
      (if (.editor-open-p module)
          (editor-close module)
          (editor-open module)))

    ;; TODO state テスト用なので後で消す
    (ig:with-popup-context-item ()
      (when (ig:menu-item "Copy" :shortcut "C-c")
        (let ((state (state module)))
          (ig:set-clipboard-text state)))
      (when (ig:menu-item "Paste" :shortcut "C-v")
        (let ((state (ig:get-clipboard-text)))
          (setf (state module) state))))

    (render-module-delete-button module)
    (utaticl.module::render-params module)))

(defmethod render-module-delete-button ((module module))
  (when (ig:button "x")
    (cmd-add (.project module) 'cmd-module-delete
             :track-id (.neko-id (.target-track (.project module)))
             :module-id (.neko-id module))))

(defmethod serialize ((self module))
  (append (call-next-method)
          `(state ,(state self))))

(defmethod start ((self module))
  (setf (.start-p self) t))

(defmethod stop ((self module))
  (setf (.start-p self) nil))

(defmethod terminate :before ((self module) &key)
  (editor-close self)
  (stop self))

(defmethod terminate ((self module) &key))

(defmethod wait-for-from-p ((self module))
  (some (lambda (connection)
          (not (.process-done (.from connection))))
        (connections-from self)))

(defmethod wait-for-to-p ((self module))
  (some (lambda (connection)
          (not (.process-done (.to connection))))
        (connections-to self)))

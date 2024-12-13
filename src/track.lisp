(in-package :utaticl.core)

(defmethod initialize-instance :after ((self track) &key)
  (let ((process-data (make-instance 'process-data
                                     :audio-input-bus-count (.nbus-audio-in self)
                                     :audio-output-bus-count (.nbus-audio-out self)))
        (gain (make-instance 'module-gain-track))
        (fader (make-instance 'module-fader-track)))
    (lane-add self (make-instance 'lane :color (.color self)))
    (setf (.process-data self) process-data)
    (module-add self gain)
    (module-add self fader)
    (start gain)
    (start fader))
  (when (string= "" (.name self))
    (setf (.name self) (name-new 'track "TRK"))))

(defmethod add-to ((self track) (to null))
  (track-add (.parent self) self))

(defmethod address ((self track))
  (concatenate 'string
               (address (.parent self))
               "/track/"
               (position self (.tracks (.parent self)))))

(defmethod after ((self track))
  (cadr (member self (.tracks (.parent self)))))

(defmethod before ((self track))
  (cadr (member self (reverse (.tracks (.parent self))))))

(defmethod erase-from ((self track) from)
  (track-delete (.parent self) self))

(defmethod lane-add ((self track) lane)
  (setf (.track lane) self)
  (setf (.lanes self) (append (.lanes self) (list lane))))

(defmethod lane-delete ((self track) lane)
  (setf (.lanes self) (remove lane (.lanes self)))
  (terminate lane))

(defmethod next ((self track))
  (cadr (member self (.tracks (.parent self)))))

(defmethod note-off-all ((self track))
  (note-off-all (.process-data self))
  (loop for track in (.tracks self)
        do (note-off-all track)))

(defmethod fader ((self track))
  (find-if (lambda (x) (typep x 'module-fader-track))
           (.modules self)))

(defmethod gain ((self track))
  (find-if (lambda (x) (typep x 'module-gain-track))
           (.modules self)))

(defmethod modules-sorted% ((self track) modules-sorted modules-processed
                            module-waiting-map)
  (loop with module-waiting = (gethash self module-waiting-map)
        for module in (.modules self)
        if (eq module module-waiting)
          do (remhash self module-waiting-map)
             (setf module-waiting nil)
        if (and (not module-waiting) (.start-p module))
          do (unless (.process-done module)
               (when (wait-for-from-p module)
                 (setf (gethash self module-waiting-map) module)
                 (return-from modules-sorted% (cons modules-sorted nil)))
               (push module modules-sorted)
               (setf (.process-done module) t))
             (when (wait-for-to-p module)
               (setf (gethash self module-waiting-map) module)
               (return-from modules-sorted% (cons modules-sorted nil))))
  (cons modules-sorted t))

(defmethod prepare ((self track))
  (setf (.module-wait-for self) nil)
  (prepare (.process-data self))
  (loop for module in (.modules self)
        do (prepare module))
  (loop for track in (.tracks self)
        do (prepare track)))

(defmethod prepare-event ((self track) start end loop-p offset-samples)
  (let ((*process-data* (.process-data self)))
    (loop for lane in (.lanes self)
          do (prepare-event lane start end loop-p offset-samples))
    (loop for track in (.tracks self)
          do (prepare-event track start end loop-p offset-samples))))

(defmethod process :around ((self track))
  (let ((*process-data* (.process-data self)))
    (call-next-method)))

(defmethod process ((self track))
  (loop with module-last = (car (last (.modules self)))
        for module in (.modules self)
        for module-wait-for = (.module-wait-for self)
        do (cond ((not (.start-p module))) ;continue
                 ((and module-wait-for
                       (not (eq module-wait-for module)))) ;continue
                 (t
                  (when module-wait-for
                    (setf (.module-wait-for self) nil))
                  (unless (.process-done module)
                    (when (wait-for-from-p module)
                      (setf (.module-wait-for self) module)
                      (loop-finish))
                    (swap-in-out *process-data*)
                    (process-connection module)
                    (process module))
                  (when (and (not (eq module module-last))
                             (wait-for-to-p module))
                    (setf (.module-wait-for self) module)
                    (loop-finish)))))
  (if (.module-wait-for self)
      self
      nil))

(defmethod .project ((self track))
  (.project (.parent self)))

(defmethod maybe-recreate-process-data ((self track) module)
  ;; in out を swap するので同じ数にする
  (let ((module-bus-count (max (.audio-input-bus-count module)
                               (.audio-output-bus-count module))))
    (when (< (.audio-input-bus-count (.process-data self))
             module-bus-count)
      (setf (.nbus-audio-in self) module-bus-count)
      (setf (.nbus-audio-out self) module-bus-count)
      (setf (.process-data self)
            (make-instance 'process-data
                           :audio-input-bus-count module-bus-count
                           :audio-output-bus-count module-bus-count)))))

(defmethod module-add ((self track) module &key before)
  (setf (.track module) self)
  (setf (.modules self)
        (if before
            (loop for x in (.modules self)
                  if (eq before x)
                    collect module
                  collect x)
            (append (.modules self)
                    (list module))))
  (maybe-recreate-process-data self module)
  (start module))

(defmethod module-delete ((self track) module)
  (setf (.track module) nil)
  (terminate module)
  (setf (.modules self) (delete module (.modules self))))

(defmethod render ((self track))
  (ig:push-id self)
  (ig:button (.name self))
  (ig:pop-id))

(defmethod .selected-p ((self track))
  (include-p (.selection-track *project*) self))

(defmethod (setf .selected-p) (value (self track))
  (add (.selection-track *project*) self))

(defmethod (setf .selected-p) :after ((value (eql t)) (self track))
  (setf (.target-track (.project self)) self))

(defmethod terminate ((self track) &key)
  (loop for module in (.modules self)
        do (stop module)
           (terminate module))
  (mapc #'terminate (.tracks self)))

(defmethod track-add ((self track) track-new &key before)
  (track-add-without-connect self track-new :before before)
  (connect (fader track-new)
           (gain self)))

(defmethod track-add-without-connect ((self track) track-new &key before)
  (setf (.parent track-new) self)
  (setf (.tracks self)
        (if before
            (loop for track in (.tracks self)
                  if (eq track before)
                    collect track-new
                  collect track)
            (append (.tracks self) (list track-new)))))

(defmethod track-delete ((self track) track-delete)
  (setf (.parent track-delete) nil)
  (setf (.tracks self) (delete track-delete (.tracks self)))
  (disconnect (fader track-delete)
              (gain self)))

(defmethod .width ((track track))
  (apply #'+ (mapcar #'.width (.lanes track))))

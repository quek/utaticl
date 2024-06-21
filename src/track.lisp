(in-package :dgw)

(defmethod initialize-instance :after ((self track) &key)
  (let ((process-data (make-instance 'process-data
                                     :num-inputs (.nbus-audio-in self)
                                     :num-outputs (.nbus-audio-out self))))
    (setf (.process-data self) process-data)))

(defmethod before ((self track))
  (map-tracks *project*
              (lambda (track acc)
                (if (eq track self)
                    (return-from before acc)
                    track))))

(defmethod next ((self track))
  (labels ((f (track)
             (aif (member self (.tracks track))
                  (cadr it)
                  (some #'f (.tracks track)))))
    (f (.master-track *project*))))

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

(defmethod parent ((self track))
  (map-tracks *project*
              (lambda (track acc)
                (if (member self (.tracks track))
                    (return-from parent track)
                    acc))))

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

(defmethod module-add ((self track) module)
  (setf (.modules self)
        (append (butlast (.modules self))
                (list module)
                (last (.modules self))))
  (initialize module)
  (start module)
  (when (zerop (c-ref (ig:get-io) ig:im-gui-io :key-shift))
    (editor-open module)))

(defmethod module-delete ((self track) module)
  (terminate module)
  (setf (.modules self) (delete module (.modules self))))

(defmethod render ((self track))
  (ig:push-id self)
  (ig:button (.name self))
  (ig:pop-id))

(defmethod (setf .select-p) :after ((value (eql t)) (self track))
  (setf (.target-track *project*) self))

(defmethod terminate ((self track))
  (loop for module in (.modules self)
        do (stop module)
           (terminate module))
  (mapc #'terminate (.tracks self)))

(defmethod track-add ((self track) track-new &key track-before)
  (setf (.tracks self)
        (if track-before
            (loop for track in (.tracks self)
                  if (eq track track-before)
                    collect track-new
                  collect track)
            (append (.tracks self) (list track-new))))
  (connect (fader track-new)
           (gain self)))

(defmethod track-delete ((self track) track-delete)
  (setf (.tracks self) (delete track-delete (.tracks self)))
  (disconnect (fader track-delete)
              (gain self)))

(defmethod unselect-all-tracks ((self track))
  (setf (.select-p self) nil)
  (mapc #'unselect-all-tracks (.tracks self)))

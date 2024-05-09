(in-package :dgw)

(defmethod initialize-instance :after ((self project) &key)
  (setf (.target-track self) (.master-track self)))

(defun cmd-add (project cmd-class &rest args)
  (push (apply #'make-instance cmd-class args) (.cmd-queue project)))

(defmethod cmd-redo ((self project))
  (let ((*project* self)
        (cmd (pop (.cmd-redo-stack self))))
    (when cmd
      (redo cmd)
      (push cmd (.cmd-undo-stack self)))))

(defmethod cmd-run ((self project))
  (let ((*project* self))
    (loop for cmd in (nreverse (.cmd-queue self))
          do (sb-thread:with-mutex ((.mutex *app*))
               (execute cmd))
             (when (.undo-p cmd)
               (setf (.cmd-redo-stack self) nil)
               (push cmd (.cmd-undo-stack self))))
    (setf (.cmd-queue self) nil)))

(defmethod cmd-undo ((self project))
  (let ((*project* self)
        (cmd (pop (.cmd-undo-stack self))))
    (when cmd
      (undo cmd)
      (push cmd (.cmd-redo-stack self)))))

(defmethod update-play-position ((self project))
  (let ((delta-sec (/ *frames-per-buffer* *sample-rate*))
        (sec-per-beat (/ 60.0d0 (.bpm self))))
    (when (and (.loop-p self) (<= (.loop-end self) (.play-start self)))
      (setf (.play-start self) (.loop-start self)))
    (setf (.play-end self) (+ (/ delta-sec sec-per-beat) (.play-start self)))
    (when (and (.loop-p self) (< (.loop-end self) (.play-end self)))
      (setf (.play-end self)
            (+ (.loop-start self) (- (.play-end self) (.loop-end self)))))))


(defun find-lane (project lane-id)
  (labels ((f (track)
             (or (find lane-id (.lanes track) :test #'equal :key #'.neko-id)
                 (some #'f (.tracks track)))))
    (f (.master-track project))))

(defun find-track (project track-id)
  (labels ((f (track)
             (if (equal track-id (.neko-id track))
                 track
                 (some #'f (.tracks track)))))
    (f (.master-track project))))

(defmethod render ((self project))
  (let ((*project* self))
    (render (.transposer self))
    (render (.arrangement self))
    (render (.piano-roll self))
    (render (.rack self))
    (render (.commander self))))

(defmethod (setf .play-p) :after (value (self project))
  (unless (.play-p self)
    (setf (.play-just-stop-p self) t)))

(defmethod process ((self project))
  (prepare (.master-track self))

  (when (.play-p self)
    (update-play-position self)

    (when (.play-just-stop-p *project*)
      (all-note-off (.master-track self))
      (setf (.play-just-stop-p *project*) nil))

    (if (< (.play-end self) (.play-start self))
        (progn
          (prepare-event (.master-track self) (.play-start self) (.loop-end self) t)
          (prepare-event (.master-track self) (.loop-start self) (.play-end self) nil))
        (prepare-event (.master-track self) (.play-start self) (.play-end self) nil)))

  (loop with tracks = (track-all self)
        with tracks-lenght = (length tracks)
          initially (loop for track in tracks
                          do (process-send self track))
        until (zerop tracks-lenght)
        do (aif (sb-concurrency:receive-message (.mailbox self))
                (process-send self it)
                (decf tracks-lenght)))

  (when (.play-p self)
    (setf (.play-start self) (.play-end self))))

(defmethod process-send ((self project) track)
  (sb-concurrency:send-message
   *thread-pool*
   (list (lambda (project track)
           (let ((*project* project))
             (sb-concurrency:send-message
              (.mailbox project) (process track))))
         self track)))

(defmethod terminate ((self project))
  (terminate (.master-track self)))

(defmethod track-all ((self project))
  (labels ((f (track)
             (cons track (loop for x in (.tracks track)
                               nconc (f x)))))
    (f (.master-track self))))

(defmethod track-name-new ((self project))
  (labels ((f (track max)
             (apply #'max (or (ppcre:register-groups-bind
                                  ((#'parse-integer n)) ("^TRACK(\\d+)" (.name track))
                                n)
                              max)
                    (mapcar (lambda (track) (f track max)) (.tracks track)))))
    (format nil "TRACK~d" (1+ (f (.master-track self) 0)))))

(defmethod unselect-all-tracks ((self project))
  (unselect-all-tracks (.master-track self)))

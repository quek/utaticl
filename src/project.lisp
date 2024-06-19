(in-package :dgw)

(defmethod initialize-instance :after ((self project) &key)
  (setf (.bpm self) 128.0)
  (setf (.target-track self) (.master-track self)))

(defmethod (setf .bpm) :after (value (self project))
  (setf (.sec-per-beat self) (/ 60.0d0 value))
  (setf (.samples-per-beat self) (* (.sec-per-beat self) (.sample-rate *config*))))


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
          do (if (.with-mutex-p cmd)
                 (sb-thread:with-mutex ((.mutex *app*))
                   (execute cmd))
                 (execute cmd))
             (when (.undo-p cmd)
               (setf (.cmd-redo-stack self) nil)
               (push cmd (.cmd-undo-stack self))
               (setf (.dirty-p self) t)))
    (setf (.cmd-queue self) nil)))

(defmethod cmd-undo ((self project))
  (let ((*project* self)
        (cmd (pop (.cmd-undo-stack self))))
    (when cmd
      (undo cmd)
      (push cmd (.cmd-redo-stack self)))))

(defmethod deserialize-after ((self project))
  (setf (.target-track self) (.master-track self)))

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

(defmethod lane-all ((self project))
  (let ((lanes nil))
    (labels ((f (track)
               (loop for lane in (.lanes track)
                     do (push lane lanes))
               (loop for x in (.tracks track)
                     do (f x))))
      (f (.master-track self)))
    (nreverse lanes)))

(defun map-lanes (project proc &optional initial-value)
  (map-tracks project
              (lambda (track acc)
                (loop for lane in (.lanes track)
                      do (setf acc (funcall proc lane acc)))
                acc)
              initial-value))

(defun map-tracks (project proc &optional initial-value)
  (labels ((f (track acc)
             (let ((acc (funcall proc track acc)))
               (loop for x in (.tracks track)
                     do (setf acc (f x acc)))
               acc)))
    (f (.master-track project) initial-value)))

(defmethod open-project ((self project))
  (stop-audio-device (.audio-device *app*))
  (multiple-value-bind (ok path)
      (ftw:get-open-file-name
       :initial-dir (substitute #\\ #\/
                                (namestring
                                 (ensure-directories-exist
                                  (merge-pathnames "user/project/" *working-directory*))))
       :filters '(("Lisp" "*.lisp") ("All" "*.*")))
    (when ok
      (let ((path (car path)))
       (with-open-file (in path :direction :input)
         (let ((project (with-serialize-context (deserialize (read in)))))
           (setf (.path project) path)
           (terminate self)
           (setf (.projects *app*)
                 (cons project (delete self (.projects *app*)))))))))
  (start-audio-device (.audio-device *app*) ))

(defmethod (setf .play-p) :after (value (self project))
  (unless (.play-p self)
    (setf (.play-just-stop-p self) t)))

(defmethod process :around ((self project))
  (let ((*project* self))
    (call-next-method)))

(defmethod process ((self project))
  (prepare (.master-track self))

  (when (.play-p self)
    (update-play-position self)

    (when (.play-just-stop-p self)
      (note-off-all (.master-track self))
      (setf (.play-just-stop-p self) nil))

    (if (< (.play-end self) (.play-start self))
        (progn
          (prepare-event (.master-track self) (.play-start self) (.loop-end self) t 0)
          (prepare-event (.master-track self) (.loop-start self) (.play-end self) nil
                         (time-to-sample *project* (- (.loop-end self) (.play-start self)))))
        (prepare-event (.master-track self) (.play-start self) (.play-end self) nil 0)))

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

(defmethod render ((self project))
  (let ((*project* self))
    (render (.transposer self))
    (render (.arrangement self))
    (render (.piano-roll self))
    (render (.rack self))
    (render (.commander self))))

(defmethod save ((self project))
  (if (.path self)
      (let ((sexp (with-serialize-context (serialize self))))
        (with-open-file (out (.path self) :direction :output :if-exists :supersede)
          (write sexp :stream out))
        (setf (.dirty-p self) nil))
      (save-as self)))

(defmethod save-as ((self project))
  (multiple-value-bind (ok path)
      (ftw:get-save-file-name
       :initial-dir (substitute #\\ #\/
                                (namestring
                                 (ensure-directories-exist
                                  (merge-pathnames "user/project/" *working-directory*))))
       :filters '(("Lisp" "*.lisp") ("All" "*.*"))
       :file (multiple-value-bind (sec min h d m y)
                 (decode-universal-time (get-universal-time))
               (declare (ignore sec min h))
               (format nil "~d~2,'0d~2,'0d.lisp" y m d)))
    (when ok
      (setf (.path self) (car path))
      (save self))))

(defmethod seq-note-name-new ((self project))
  (let ((max 0))
    (labels
        ((f (track)
           (loop for lane in (.lanes track)
                 do (loop for clip in (.clips lane)
                          for seq = (.seq clip)
                          if (typep seq 'seq-note)
                            do (setf max
                                     (max (or (ppcre:register-groups-bind
                                                  ((#'parse-integer n)) ("^SEQ(\\d+)" (.name seq))
                                                n)
                                              max)))))
           (loop for x in (.tracks track)
                 do (f x))))
      (f (.master-track self))
      (format nil "SEQ~d" (1+ max)))))

(defmethod terminate ((self project))
  (terminate (.master-track self)))

(defmethod time-to-sample ((self project) time)
  (declare (double-float time))
  (round (* time (.samples-per-beat self))))


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

(defmethod track-group-name-new ((self project))
  (labels ((f (track max)
             (apply #'max (or (ppcre:register-groups-bind
                                  ((#'parse-integer n)) ("^GROUP(\\d+)" (.name track))
                                n)
                              max)
                    (mapcar (lambda (track) (f track max)) (.tracks track)))))
    (format nil "GROUP~d" (1+ (f (.master-track self) 0)))))

(defmethod tracks-selected ((self project))
  (map-tracks self
              (lambda (track acc)
                (if (.select-p track)
                    (cons track acc)
                    acc))))

(defmethod unselect-all-tracks ((self project))
  (unselect-all-tracks (.master-track self)))

(defmethod update-play-position ((self project))
  (let ((delta-sec (/ (.frames-per-buffer *config*) (.sample-rate *config*)))
        (sec-per-beat (/ 60.0d0 (.bpm self))))
    (when (and (.loop-p self) (<= (.loop-end self) (.play-start self)))
      (setf (.play-start self) (.loop-start self)))
    (setf (.play-end self) (+ (/ delta-sec sec-per-beat) (.play-start self)))
    (when (and (.loop-p self) (< (.loop-end self) (.play-end self)))
      (setf (.play-end self)
            (+ (.loop-start self) (- (.play-end self) (.loop-end self)))))))

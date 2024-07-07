(in-package :dgw)

(defmethod initialize-instance :after ((self project) &key)
  (setf (.bpm self) 128.0)
  (let ((arrangement  (make-instance 'arrangement :project self))
        (sceen-matrix  (make-instance 'sceen-matrix))
        (commander (make-instance 'commander :project self))
        ;; project は (setf .master-track) :after で setf
        (master-track (make-instance 'master-track))
        (rack (make-instance 'rack :project self))
        (transposer (make-instance 'transposer :project self)))
    (setf (.arrangement self) arrangement)
    (setf (.sceen-matrix self) sceen-matrix)
    (setf (.commander self) commander)
    (setf (.master-track self) master-track)
    (setf (.rack self) rack)
    (setf (.transposer self) transposer)
    (setf (.target-track self) master-track)))

(defmethod (setf .bpm) :after (value (self project))
  (setf (.sec-per-beat self) (/ 60.0d0 value))
  (setf (.samples-per-beat self)
        (* (.sec-per-beat self) (.sample-rate *config*))))


(defmethod cmd-add ((project project) cmd-class &rest args)
  (push (apply #'make-instance cmd-class args) (.cmd-queue project)))

(defmethod cmd-redo ((self project))
  (let ((cmd (pop (.cmd-redo-stack self))))
    (when cmd
      (redo cmd self)
      (push cmd (.cmd-undo-stack self)))))

(defmethod cmd-run ((self project))
  (loop for cmd in (nreverse (.cmd-queue self))
        do (if (.with-mutex-p cmd)
               (sb-thread:with-mutex ((.mutex *app*))
                 (execute cmd self))
               (execute cmd self))
           (when (.undo-p cmd)
             (setf (.cmd-redo-stack self) nil)
             (push cmd (.cmd-undo-stack self))
             (setf (.dirty-p self) t)))
  (setf (.cmd-queue self) nil))

(defmethod cmd-undo ((self project))
  (let ((cmd (pop (.cmd-undo-stack self))))
    (when cmd
      (undo cmd self)
      (push cmd (.cmd-redo-stack self)))))

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

(defmethod latency-compute ((self project))
  (let ((modules-sorted (modules-sorted self)))
    (loop for module in modules-sorted
          for latency = (.latency module)
          do (loop for x in modules-sorted
                   until (eq module x)
                   if (eq (.track module) (.track x))
                     do (setf latency (+ (.latency module) (.latency-pdc x))))
             (loop for connection in (.connections module)
                   if (eq module (.to connection))
                     do (setf latency (max latency
                                           (.latency-pdc (.from connection)))))
             (setf (.latency-pdc module) latency))))

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

(defmethod (setf .master-track) :after (master-track (self project))
  (setf (.project master-track) self)
  (setf (.target-track self) master-track))

(defmethod modules-sorted ((self project))
  (let ((modules-sorted nil)
        (modules-processed nil)
        (module-waiting-map (make-hash-table)))
    (prepare (.master-track self))
    (loop until (map-tracks self
                            (lambda (track acc)
                              (destructuring-bind (sorted . ret)
                                  (modules-sorted% track
                                                   modules-sorted
                                                   modules-processed
                                                   module-waiting-map)
                                (setf modules-sorted sorted)
                                (and ret acc)))
                            t))
    (nreverse modules-sorted)))

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
          (let ((project (with-serialize-context () (deserialize (read in)))))
            (setf (.path project) path)
            (terminate self)
            (setf (.projects *app*)
                  (cons project (delete self (.projects *app*)))))))))
  (start-audio-device (.audio-device *app*) ))

(defmethod (setf .piano-roll) :after (piano-roll (self project))
  (setf (.project piano-roll) self))

(defmethod (setf .play-p) :after (value (self project))
  (unless (.play-p self)
    (setf (.play-just-stop-p self) t))
  (unless value
    (setf (.play-p (.sceen-matrix self)) nil)))

(defmethod process ((self project))
  (prepare (.master-track self))

  (when (.play-p self)
    (update-play-position self)

    (when (.play-just-stop-p self)
      (note-off-all (.master-track self))
      (setf (.play-just-stop-p self) nil))

    (if (< (.play-start self) (.play-end self))
        (progn
          (prepare-event (.master-track self) (.play-start self) (.play-end self) nil 0)
          (prepare-event (.sceen-matrix self) (.play-start self) (.play-end self) nil 0))
        (progn
          (prepare-event (.master-track self) (.play-start self) (.loop-end self) t 0)
          (prepare-event (.master-track self) (.loop-start self) (.play-end self) nil
                         (time-to-sample self (- (.loop-end self) (.play-start self))))
          (prepare-event (.sceen-matrix self) (.play-start self) (.loop-end self) t 0)
          (prepare-event (.sceen-matrix self) (.loop-start self) (.play-end self) nil
                         (time-to-sample self (- (.loop-end self) (.play-start self)))))))

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
           (sb-concurrency:send-message
            (.mailbox project) (process track)))
         self track)))

(defmethod render ((self project))
  (render (.transposer self))
  (render (.arrangement self))
  (render (.sceen-matrix self))
  (render (.piano-roll self))
  (render (.rack self))
  (render (.commander self)))

(defmethod save ((self project))
  (if (.path self)
      (let ((sexp (with-serialize-context () (serialize self))))
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
      (let* ((path (car path)))
        (unless (string-equal "lisp" (pathname-type path))
          (setf path (namestring (merge-pathnames path "_.lisp"))))
        (setf (.path self) path))
      (save self))))

(defmethod (setf .sceen-matrix) :after ((sceen-matrix sceen-matrix) (project project))
  (setf (.project sceen-matrix) project))

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

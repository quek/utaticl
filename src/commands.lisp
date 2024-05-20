(in-package :dgw)

(defmacro defcommand (name super slots &optional class-options)
  `(defclass ,name ,super
     ,slots
     ,@(when class-options `(, class-options))))

(defclass command ()
  ((undo-p :initarg :undo-p :initform t :accessor .undo-p)
   (execute-after :initarg :execute-after :initform nil :accessor .execute-after)
   (with-mutex-p :initarg :with-mutex-p :initform t :accessor .with-mutex-p)))

(defmethod execute :after ((self command))
  (let ((f (.execute-after self)))
    (when f
      (setf (.execute-after self) nil)
      (funcall f self))))

(defmethod redo ((self command))
  (execute self))

(defcommand cmd-audio-engine-start (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-audio-engine-start))
  (start-audio-device (.audio-device *app*)))

(defcommand cmd-audio-engine-stop (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-audio-engine-stop))
  (stop-audio-device (.audio-device *app*)))

(defcommand cmd-clip-add (command)
  ((time :initarg :time :accessor .time)
   (lane-id :initarg :lane-id :accessor .lane-id)
   (clip-id :accessor .clip-id)))

(defmethod execute ((self cmd-clip-add))
  (let ((lane (find-lane *project* (.lane-id self)))
        (clip (make-instance 'clip-note :time (.time self))))
    (setf (.name (.seq clip)) (seq-note-name-new *project*))
    (setf (.clip-id self) (.neko-id clip))
    (clip-add lane clip)))

(defmethod undo ((self cmd-clip-add))
  (let* ((lane (find-lane *project* (.lane-id self)))
         (clip (find (.clip-id self) (.clips lane)
                     :key #'.neko-id :test #'equal)))
    (clip-delete lane clip)
    (swhen (.piano-roll *project*)
      (when (and it (eq clip (.clip it)))
        (setf it nil)))))

(defcommand cmd-clip-delete (command)
  ((clip :accessor .clip)
   (clip-id :initarg :clip-id :accessor .clip-id)
   (lane-id :accessor .lane-id)))

(defmethod execute ((self cmd-clip-delete))
  (let* ((clip (find-neko (.clip-id self)))
         (lane (lane clip)))
    (setf (.clip self) (with-serialize-context (serialize clip)))
    (setf (.lane-id self) (.neko-id lane))
    (clip-delete lane clip)
    (swhen (.piano-roll *project*)
      (when (and it (eq clip (.clip it)))
        (setf it nil)))))

(defmethod undo ((self cmd-clip-delete))
  (let* ((clip (deserialize (.clip self)))
         (lane (find-neko (.lane-id self))))
    (clip-add lane clip)))

(defcommand cmd-clips-d&d-copy (command)
  ((clips :initarg :clips :accessor .clips)
   (clip-ids :accessor .clip-ids)
   (lane-ids :initarg :lane-ids :accessor .lane-ids)))

(defmethod initialize-instance :after ((self cmd-clips-d&d-copy) &key clips)
  (setf (.clip-ids self) (mapcar #'.neko-id clips))
  (setf (.clips self)
        (with-serialize-context (serialize clips))))

(defmethod execute ((self cmd-clips-d&d-copy))
  ;; ドラッグ中の表示が確定されるだけなので、何もしない。
  )

(defmethod undo ((self cmd-clips-d&d-copy))
  (loop for clip-id in (.clip-ids self)
        for lane-id in (.lane-ids self)
        for clip = (find-neko clip-id)
        for lane = (find-neko lane-id)
        do (clip-delete lane clip)))

(defmethod redo ((self cmd-clips-d&d-copy))
  (loop for clip in (with-serialize-context (deserialize (.clips self)))
        for lane-id in (.lane-ids self)
        for lane = (find-neko lane-id)
        do (clip-add lane clip)))

(defcommand cmd-clips-d&d-move (command)
  ((clip-ids :initarg :clips :accessor .clip-ids)
   (lane-ids :initarg :lane-ids :accessor .lane-ids)
   (times-from :accessor .times-from)
   (times-to :initarg :times-to :accessor .times-to)
   (lane-ids-to :initarg :lane-ids-to :accessor .lane-ids-to)))

(defmethod initialize-instance :after ((self cmd-clips-d&d-move) &key clips)
  (setf (.times-from self) (mapcar #'.time clips))
  (setf (.clip-ids self) (mapcar #'.neko-id clips)))

(defmethod execute ((self cmd-clips-d&d-move))
  (loop for clip-id in (.clip-ids self)
        for clip = (find-neko clip-id)
        for time-to in (.times-to self)
        for lane-id-from in (.lane-ids self)
        for lane-from = (find-neko lane-id-from)
        for lane-id-to in (.lane-ids-to self)
        for lane-to = (find-neko lane-id-to)
        do (setf (.time clip) time-to)
           (unless (eq lane-from lane-to)
             (clip-delete lane-from clip)
             (clip-add lane-to clip))))

(defmethod undo ((self cmd-clips-d&d-move))
  (loop for clip-id in (.clip-ids self)
        for clip = (find-neko clip-id)
        for time-from in (.times-from self)
        for lane-id-from in (.lane-ids self)
        for lane-from = (find-neko lane-id-from)
        for lane-id-to in (.lane-ids-to self)
        for lane-to = (find-neko lane-id-to)
        do (setf (.time clip) time-from)
           (unless (eq lane-from lane-to)
             (clip-delete lane-to clip)
             (clip-add lane-from clip))))

(defcommand cmd-module-add (command)
  ((track-id :initarg :track-id :accessor .track-id)
   (plugin-info :initarg :plugin-info :accessor .plugin-info)))

(defmethod execute ((self cmd-module-add))
  (let ((track (find-track *project* (.track-id self)))
        (module (plugin-load (.plugin-info self))))
    (module-add track module)))

(defcommand cmd-note-add (command)
  ((clip-id :initarg :clip-id :accessor .clip-id)
   (time :initarg :time :accessor .time)
   (key :initarg :key :accessor .key)
   (note-id :accessor .note-id)))

(defmethod execute ((self cmd-note-add))
  (let ((clip (find-neko (.clip-id self)))
        (note (make-instance 'note :time (.time self) :key (.key self))))
    (setf (.note-id self) (.neko-id note))
    (note-add clip note)))

(defmethod undo ((self cmd-note-add))
  (let* ((clip (find-neko (.clip-id self)))
         (note (find-neko (.note-id self))))
    (note-delete clip note)))

(defcommand cmd-module-delete (command)
  ((track-id :initarg :track-id :accessor .track-id)
   (module-id :initarg :module-id :accessor .module-id)))

(defmethod execute ((self cmd-module-delete))
  (let* ((track (find-track *project* (.track-id self)))
        (module (find (.module-id self) (.modules track) :key #'.neko-id)))
    (module-delete track module)))

(defcommand cmd-open (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-open))
  (open-project *project*))

(defcommand cmd-plugin-scan (command)
  ())

(defmethod execute ((self cmd-plugin-scan))
  (let ((path (merge-pathnames "user/config/plugins.lisp" *working-directory*)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (loop for plugin-info in (vst3::plugin-scan-vst3)
            do (write (serialize plugin-info) :stream out)
               (terpri out)))))

(defcommand cmd-redo (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-redo))
  (cmd-redo *project*))

(defcommand cmd-save (command)
  ()
  (:default-initargs :undo-p nil :with-mutex-p nil))

(defmethod execute ((self cmd-save))
  (save *project*))

(defcommand cmd-save-as (command)
  ()
  (:default-initargs :undo-p nil :with-mutex-p nil))

(defmethod execute ((self cmd-save-as))
  (save-as *project*))

(defcommand cmd-track-add (command)
  ((track-id-before :initarg :track-id-before
                    :initform nil
                    :accessor .track-id-before)
   (track-id-new :accessor .track-id-new)
   (track-id-parent :initarg :track-id-parent
                    :accessor .track-id-parent)))

(defmethod execute ((self cmd-track-add))
  (let ((track-before (find-neko (.track-id-before self)))
        (track-new (make-instance 'track :name (track-name-new *project*)))
        (track-parent (find-neko (.track-id-parent self))))
    (setf (.track-id-new self) (.neko-id track-new))
    (track-add track-parent track-new :track-before track-before)))

(defmethod undo ((self cmd-track-add))
  (let ((track-new (find-neko (.track-id-new self)))
        (track-parent (find-neko (.track-id-parent self))))
    (track-delete track-parent track-new))
  )

(defcommand cmd-undo (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-undo))
  (cmd-undo *project*))

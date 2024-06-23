(in-package :dgw)

(defmacro defcommand (name super slots &optional class-options)
  `(defclass ,name ,super
     ,slots
     ,@(when class-options `(, class-options))))

(defclass command ()
  ((undo-p :initarg :undo-p :initform t :accessor .undo-p)
   (execute-after :initarg :execute-after :initform nil :accessor .execute-after)
   (with-mutex-p :initarg :with-mutex-p :initform t :accessor .with-mutex-p)))

(defmethod execute :after ((self command) project)
  (let ((f (.execute-after self)))
    (when f
      (setf (.execute-after self) nil)
      (funcall f self))))

(defmethod redo ((self command) project)
  (execute self project))

(defcommand cmd-audio-engine-start (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-audio-engine-start) project)
  (start-audio-device (.audio-device *app*)))

(defcommand cmd-audio-engine-stop (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-audio-engine-stop) project)
  (stop-audio-device (.audio-device *app*)))

(defcommand cmd-clip-add (command)
  ((time :initarg :time :accessor .time)
   (lane-id :initarg :lane-id :accessor .lane-id)
   (clip-id :accessor .clip-id)))

(defmethod execute ((self cmd-clip-add) project)
  (let ((lane (find-lane project (.lane-id self)))
        (clip (make-instance 'clip-note :time (.time self))))
    (setf (.name (.seq clip)) (seq-note-name-new project))
    (setf (.clip-id self) (.neko-id clip))
    (clip-add lane clip)))

(defmethod undo ((self cmd-clip-add) project)
  (let* ((lane (find-lane project (.lane-id self)))
         (clip (find (.clip-id self) (.clips lane)
                     :key #'.neko-id :test #'equal)))
    (clip-delete lane clip)
    (swhen (.piano-roll project)
      (when (and it (eq clip (.clip it)))
        (setf it nil)))))

(defcommand cmd-clip-delete (command)
  ((clip :accessor .clip)
   (clip-id :initarg :clip-id :accessor .clip-id)
   (lane-id :accessor .lane-id)))

(defmethod execute ((self cmd-clip-delete) project)
  (let* ((clip (find-neko (.clip-id self)))
         (lane (lane clip)))
    (setf (.clip self) (with-serialize-context () (serialize clip)))
    (setf (.lane-id self) (.neko-id lane))
    (clip-delete lane clip)
    (swhen (.piano-roll project)
      (when (and it (eq clip (.clip it)))
        (setf it nil)))))

(defmethod undo ((self cmd-clip-delete) project)
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
        (with-serialize-context () (serialize clips))))

(defmethod execute ((self cmd-clips-d&d-copy) project)
  ;; ドラッグ中の表示が確定されるだけなので、何もしない。
  )

(defmethod undo ((self cmd-clips-d&d-copy) project)
  (loop for clip-id in (.clip-ids self)
        for lane-id in (.lane-ids self)
        for clip = (find-neko clip-id)
        for lane = (find-neko lane-id)
        do (clip-delete lane clip)))

(defmethod redo ((self cmd-clips-d&d-copy) project)
  (loop for clip in (with-serialize-context () (deserialize (.clips self)))
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

(defmethod execute ((self cmd-clips-d&d-move) project)
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

(defmethod undo ((self cmd-clips-d&d-move) project)
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

(defcommand cmd-clips-delete (command)
  ((clips :initarg :clips :accessor .clips)
   (lanes :initform :nil :accessor .lanes)))

(defmethod execute ((self cmd-clips-delete) project)
  (setf (.lanes self)
        (loop for clip in (.clips self)
              for lane = (.lane clip)
              do (clip-delete lane clip)
              collect lane))
  (let ((serialized (with-serialize-context () (serialize (.clips self)))))
    (loop for clip in (.clips self)
          do (terminate clip))
    (setf (.clips self) serialized)))

(defmethod undo ((self cmd-clips-delete) project)
  (setf (.clips self) (with-serialize-context () (deserialize (.clips self))))
  (loop for clip in (.clips self)
        for lane in (.lanes self)
        do (clip-add lane clip)))

(defcommand cmd-latency-compute (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-latency-compute) project)
  (latency-compute project))

(defcommand cmd-module-add (command)
  ((track-id :initarg :track-id :accessor .track-id)
   (plugin-info :initarg :plugin-info :accessor .plugin-info)
   (before :initarg :before :initform nil :accessor .before)))

(defmethod execute ((self cmd-module-add) project)
  (let ((track (find-track project (.track-id self)))
        (module (plugin-load (.plugin-info self))))
    (module-add track module :before (.before self))))

(defcommand cmd-note-add (command)
  ((clip-id :initarg :clip-id :accessor .clip-id)
   (time :initarg :time :accessor .time)
   (key :initarg :key :accessor .key)
   (duration :initarg :duration :accessor .duration)
   (note-id :accessor .note-id)))

(defmethod execute ((self cmd-note-add) project)
  (let ((clip (find-neko (.clip-id self)))
        (note (make-instance 'note
                             :time (.time self)
                             :key (.key self)
                             :duration (.duration self))))
    (setf (.note-id self) (.neko-id note))
    (note-add clip note)))

(defmethod undo ((self cmd-note-add) project)
  (let* ((clip (find-neko (.clip-id self)))
         (note (find-neko (.note-id self))))
    (note-delete clip note)))

(defcommand cmd-note-delete (command)
  ((clip-id :initarg :clip-id :accessor .clip-id)
   (note-id :accessor .note-id)
   (note :initarg :note :accessor .note)))

(defmethod initialize-instance :after ((self cmd-note-delete) &key note)
  (setf (.note-id self) (.neko-id note))
  (setf (.note self) (with-serialize-context () (serialize note))))

(defmethod execute ((self cmd-note-delete) project)
  (let ((clip (find-neko (.clip-id self)))
        (note (find-neko (.note-id self))))
    (note-delete clip note)))

(defmethod undo ((self cmd-note-delete) project)
  (let* ((clip (find-neko (.clip-id self)))
         (note (with-serialize-context () (deserialize (.note self)))))
    (note-delete clip note)))

(defcommand cmd-notes-d&d-copy (command)
  ((notes :initarg :notes :accessor .notes)
   (note-ids :accessor .note-ids)
   (clip-id :initarg :clip-id :accessor .clip-id)))

(defmethod initialize-instance :after ((self cmd-notes-d&d-copy) &key notes)
  (setf (.note-ids self) (mapcar #'.neko-id notes))
  (setf (.notes self) (with-serialize-context () (serialize notes))))

(defmethod execute ((self cmd-notes-d&d-copy) project)
  ;; ドラッグ中の表示が確定されるだけなので、何もしない。
  )

(defmethod undo ((self cmd-notes-d&d-copy) project)
  (loop with clip = (find-neko (.clip-id self))
        for note-id in (.note-ids self)
        for note = (find-neko note-id)
        do (note-delete clip note)))

(defmethod redo ((self cmd-notes-d&d-copy) project)
  (loop with clip = (find-neko (.clip-id self))
        for note in (with-serialize-context () (deserialize (.notes self)))
        do (note-add clip note)))

(defcommand cmd-notes-d&d-move (command)
  ((note-ids :initarg :notes :accessor .note-ids)
   (times-from :accessor .times-from)
   (times-to :initarg :times-to :accessor .times-to)
   (keys-from :accessor .keys-from)
   (keys-to :initarg :keys-to :accessor .keys-to)))

(defmethod initialize-instance :after ((self cmd-notes-d&d-move) &key notes)
  (setf (.times-from self) (mapcar #'.time notes))
  (setf (.keys-from self) (mapcar #'.key notes))
  (setf (.note-ids self) (mapcar #'.neko-id notes)))

(defmethod execute ((self cmd-notes-d&d-move) project)
  (loop for note-id in (.note-ids self)
        for note = (find-neko note-id)
        for time-to in (.times-to self)
        for key-to in (.keys-to self)
        do (setf (.time note) time-to)
           (setf (.key note) key-to)))

(defmethod undo ((self cmd-notes-d&d-move) project)
  (loop for note-id in (.note-ids self)
        for note = (find-neko note-id)
        for time-from in (.times-from self)
        for key-from in (.keys-from self)
        do (setf (.time note) time-from)
           (setf (.key note) key-from)))

(defcommand cmd-notes-delete (command)
  ((notes :initarg :notes :accessor .notes)
   (clip :initarg :clip :accessor .clip)
   (notes-undo :accessor .notes-undo)))

(defmethod execute ((self cmd-notes-delete) project)
  (setf (.notes-undo self)
        (with-serialize-context () (serialize (.notes self))))
  (loop with clip = (.clip self)
        for note in (.notes self)
        do (note-delete clip note)))

(defmethod undo ((self cmd-notes-delete) project)
  (setf (.notes self)
        (with-serialize-context () (deserialize (.notes-undo self))))
  (loop with clip = (.clip self)
        for note in (.notes self)
        do (note-add clip note)))

(defcommand cmd-notes-duplicate (command)
  ((notes :initarg :notes :accessor .notes)
   (clip :initarg :clip :accessor .clip)
   (notes-undo :accessor .notes-undo)))

(defmethod execute ((self cmd-notes-duplicate) project)
  (let* ((time-min most-positive-double-float)
         (time-max .0d0)
         (notes (loop for note in (.notes self)
                      if (< (.time note) time-min)
                        do (setf time-min (.time note))
                      if (< time-max (time-end note))
                        do (setf time-max (time-end note))
                      collect (copy note)))
         (time-delta (- time-max time-min)))
    (loop with clip = (.clip self)
          for note in notes
          do (incf (.time note) time-delta)
             (note-add clip note))
    (setf (.notes-undo self) notes)))

(defmethod undo ((self cmd-notes-duplicate) project)
  (loop with clip = (.clip self)
        for note in (.notes-undo self)
        do (note-delete clip note)))

(defcommand cmd-notes-end-change (command)
  ((notes-id :initarg :notes :accessor .notes-id)
   (delta :initarg :delta :accessor .delta)))

(defmethod initialize-instance :after ((self cmd-notes-end-change) &key notes)
  (setf (.notes-id self) (mapcar #'.neko-id notes)))

(defmethod execute ((self cmd-notes-end-change) project)
  (loop with delta = (.delta self)
        for note-id in (.notes-id self)
        for note = (find-neko note-id)
        do (incf (.duration note) delta)))

(defmethod undo ((self cmd-notes-end-change) project)
  (loop with delta = (.delta self)
        for note-id in (.notes-id self)
        for note = (find-neko note-id)
        do (decf (.duration note) delta)))

(defcommand cmd-notes-start-change (command)
  ((notes-id :initarg :notes :accessor .notes-id)
   (delta :initarg :delta :accessor .delta)))

(defmethod initialize-instance :after ((self cmd-notes-start-change) &key notes)
  (setf (.notes-id self) (mapcar #'.neko-id notes)))

(defmethod execute ((self cmd-notes-start-change) project)
  (loop with delta = (.delta self)
        for note-id in (.notes-id self)
        for note = (find-neko note-id)
        do (decf (.time note) delta)
           (incf (.duration note) delta)))

(defmethod undo ((self cmd-notes-start-change) project)
  (loop with delta = (.delta self)
        for note-id in (.notes-id self)
        for note = (find-neko note-id)
        do (incf (.time note) delta)
           (decf (.duration note) delta)))

(defcommand cmd-module-delete (command)
  ((track-id :initarg :track-id :accessor .track-id)
   (module-id :initarg :module-id :accessor .module-id)))

(defmethod execute ((self cmd-module-delete) project)
  (let* ((track (find-track project (.track-id self)))
        (module (find (.module-id self) (.modules track) :key #'.neko-id)))
    (module-delete track module)))

(defcommand cmd-open (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-open) project)
  (open-project project))

(defcommand cmd-plugin-scan (command)
  ())

(defmethod execute ((self cmd-plugin-scan) project)
  (let ((path (merge-pathnames "user/config/plugins.lisp" *working-directory*)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (loop for plugin-info in (vst3::plugin-scan-vst3)
            do (write (serialize plugin-info) :stream out)
               (terpri out)))))

(defcommand cmd-redo (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-redo) project)
  (cmd-redo project))

(defcommand cmd-save (command)
  ()
  (:default-initargs :undo-p nil :with-mutex-p nil))

(defmethod execute ((self cmd-save) project)
  (save project))

(defcommand cmd-save-as (command)
  ()
  (:default-initargs :undo-p nil :with-mutex-p nil))

(defmethod execute ((self cmd-save-as) project)
  (save-as project))

(defcommand cmd-track-add (command)
  ((track-id-before :initarg :track-id-before
                    :initform nil
                    :accessor .track-id-before)
   (track-id-new :accessor .track-id-new)
   (track-id-parent :initarg :track-id-parent
                    :accessor .track-id-parent)))

(defmethod execute ((self cmd-track-add) project)
  (let ((track-before (find-neko (.track-id-before self)))
        (track-new (make-instance 'track :name (track-name-new project)))
        (track-parent (find-neko (.track-id-parent self))))
    (setf (.track-id-new self) (.neko-id track-new))
    (track-add track-parent track-new :before track-before)))

(defmethod undo ((self cmd-track-add) project)
  (let ((track-new (find-neko (.track-id-new self)))
        (track-parent (find-neko (.track-id-parent self))))
    (track-delete track-parent track-new)))

(defcommand cmd-tracks-group (command)
  ((tracks :initarg :tracks :accessor .tracks)
   (track-group :accessor .track-group)
   (parents :accessor .parents)
   (tracks-before :accessor .tracks-before)))

(defmethod execute ((self cmd-tracks-group) project)
  (let* ((track-group (make-instance 'track :name (track-group-name-new project)))
         (parents (mapcar #'parent (.tracks self))))
    (setf (.track-group self) track-group)
    (setf (.parents self) parents)
    (setf (.tracks-before self) (mapcar #'next (.tracks self)))
    (track-add (car parents) track-group :before (car (.tracks self)))
    (loop for track in (.tracks self)
          for parent in parents
          do (track-delete parent track)
          do (track-add track-group track))))

(defmethod undo ((self cmd-tracks-group) project)
  (let ((track-group (.track-group self)))
    (track-delete (parent track-group) track-group)
    (loop for track in (.tracks self)
          for parent in (.parents self)
          for track-before in (.tracks-before self)
          do (track-delete track-group track)
             (track-add parent track :before track-before))))

(defcommand cmd-undo (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-undo) project)
  (cmd-undo project))

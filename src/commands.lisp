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

(defcommand cmd-audio-device-window (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-audio-device-window) project)
  (setf (.render-audio-device-window-p *app*) t))

(defcommand cmd-audio-engine-stop (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-audio-engine-stop) project)
  (stop-audio-device (.audio-device *app*)))

(defcommand cmd-clip-add (command)
  ((clip :initarg :clip :accessor .clip)
   (lane :initarg :lane :accessor .lane)
   (sceen :initarg :sceen :initform nil :accessor .sceen)))

(defmethod execute ((self cmd-clip-add) project)
  (let ((clip (.clip self))
        (lane (.lane self))
        (sceen (.sceen self)))
    (if sceen
        (clip-add sceen clip :lane lane)
        (clip-add lane clip))))

(defmethod undo ((self cmd-clip-add) project)
  (let ((clip (.clip self))
        (lane (.lane self))
        (sceen (.sceen self)))
    (if lane
        (clip-delete lane clip)
        (clip-delete sceen clip))
    (setf (.clip self) (with-serialize-context ()
                         (serialize (.clip self))))
    (terminate clip)
    (swhen (.piano-roll project)
      (when (and it (eq clip (.clip it)))
        (setf it nil)))))

(defmethod redo ((self cmd-clip-add) project)
  (setf (.clip self) (with-serialize-context ()
                       (deserialize (.clip self))))
  (call-next-method))

(defcommand cmd-clip-audio-add (command)
  ((clip :accessor .clip)
   (lane :initarg :lane :accessor .lane)
   (path :initarg :path :accessor .path)
   (time :initarg :time :accessor .time)))

(defmethod execute ((self cmd-clip-audio-add) project)
  (let ((clip (make-instance 'clip-audio
                             :time (.time self)
                             :path (.path self)
                             :color (.color (.lane self)))))
    (clip-add (.lane self) clip)
    (setf (.clip self) clip)))

(defmethod undo ((self cmd-clip-audio-add) project)
  (clip-delete (.lane self) (.clip self)))

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

(defcommand cmd-clips-add (command)
  ((clips :initarg :clips :accessor .clips)))

(defmethod execute ((self cmd-clips-add) project)
  (loop for clip in (.clips self)
        for sceen = (.sceen clip)
        do (clip-add (or sceen (.lane clip)) clip)))

(defmethod undo ((self cmd-clips-add) project)
  (loop for clip in (.clips self)
        for sceen = (.sceen clip)
        do (clip-delete (or sceen (.lane clip)) clip)
           (terminate clip))
  (setf (.clips self) (with-serialize-context ()
                        (serialize (.clips self)))))

(defmethod redo ((self cmd-clips-add) project)
  (setf (.clips self) (with-serialize-context ()
                        (deserialize (.clips self))))
  (execute self project))

(defcommand cmd-clips-d&d-copy (command)
  ((clips :initarg :clips :accessor .clips)
   (clip-ids :accessor .clip-ids)
   (lane-ids :initarg :lane-ids :accessor .lane-ids)))

(defmethod execute ((self cmd-clips-d&d-copy) project)
  ;; ドラッグ中の表示が確定されるだけなので、何もしない。
  )

(defmethod undo ((self cmd-clips-d&d-copy) project)
  (let ((clips-serialized (with-serialize-context ()
                            (serialize (.clips self)))))
    (loop for clip in (.clips self)
          for sceen = (.sceen clip)
          for lane = (.lane clip)
          do (clip-delete (or sceen lane) clip)
             (terminate clip))
    (setf (.clips self) clips-serialized)))

(defmethod redo ((self cmd-clips-d&d-copy) project)
  (setf (.clips self) (with-serialize-context ()
                        (deserialize (.clips self))))
  (execute self project ))

(defcommand cmd-clips-d&d-move (command)
  ((clips :initarg :clips :accessor .clips)
   (times-from :accessor .times-from)
   (lanes-from :accessor .lanes-from)
   (sceens-from :accessor .sceens-from)
   (times-to :initarg :times-to :accessor .times-to)
   (lanes-to :initarg :lanes-to :accessor .lanes-to)
   (sceens-to :initarg :sceens-to :accessor .sceens-to)))

(defmethod initialize-instance :after ((self cmd-clips-d&d-move) &key clips)
  (setf (.times-from self) (mapcar #'.time clips))
  (setf (.lanes-from self) (mapcar #'.lane clips))
  (setf (.sceens-from self) (mapcar #'.sceen clips)))

(defmethod execute ((self cmd-clips-d&d-move) project)
  (loop for clip in (.clips self)
        for time-to in (.times-to self)
        for lane-to in (.lanes-to self)
        for lane-from in (.lanes-from self)
        for sceen-to in (.sceens-to self)
        for sceen-from in (.sceens-from self)
        do (setf (.time clip) time-to)
           (unless (and (eq lane-from lane-to)
                        (eq sceen-from sceen-to))
             (clip-delete (or sceen-from lane-from) clip)
             (if sceen-to
                 (clip-add sceen-to clip :lane lane-to)
                 (clip-add lane-to clip)))))

(defmethod undo ((self cmd-clips-d&d-move) project)
  (loop for clip in (.clips self)
        for time-from in (.times-from self)
        for lane-from in (.lanes-from self)
        for lane-to in (.lanes-to self)
        for sceen-from in (.sceens-from self)
        for sceen-to in (.sceens-to self)
        do (setf (.time clip) time-from)
           (unless (and (eq lane-from lane-to)
                        (eq sceen-from sceen-to))
             (clip-delete (or sceen-to lane-to) clip)
             (if sceen-from
                 (clip-add sceen-from clip :lane lane-from)
                 (clip-add lane-from clip)))))

(defcommand cmd-clips-d&d-move-from-arrangement-to-sceen-matrix (command)
  ((clips-from :initarg :clips-from :accessor .clips-from)
   (clips-to :initarg :clips-to :initform nil :accessor .clips-to)))

(defmethod initialize-instance :after ((self cmd-clips-d&d-move-from-arrangement-to-sceen-matrix)
                                       &key sceen-to lane-from lane-to)
  (loop with lane-delta = (diff lane-from lane-to)
        for clip-from in (.clips-from self)
        for clip-to = (copy clip-from)
        do (setf (.sceen clip-to) sceen-to)
           (setf (.lane clip-to) (relative-at (.lane clip-to) lane-delta))
           (push (.clips-to self) clip-to)))

(defmethod execute ((self cmd-clips-d&d-move-from-arrangement-to-sceen-matrix) project)
  (let ((clips-from-serialized (with-serialize-context ()
                                 (serialize (.clips-from self)))))
    (loop for clip-from in (.clips-from self)
          do (clip-delete (.lane clip-from) clip-from))
    (setf (.clips-from self) clips-from-serialized))
  (loop for clip-to in (.clips-to self)
        do (clip-add (.sceen clip-to) clip-to)))

(defmethod undo ((self cmd-clips-d&d-move-from-arrangement-to-sceen-matrix) project)
  (let ((clips-to-serialized (with-serialize-context ()
                               (serialize (.clips-to self)))))
    (loop for clip-to in (.clips-to self)
          do (clip-delete (.sceen clip-to) clip-to))
    (setf (.clips-to self) clips-to-serialized))
  (setf (.clips-from self)
        (loop for clip-from in (with-serialize-context ()
                                 (deserialize (.clips-from self)))
              do (clip-add (.lane clip-from) clip-from)
              collect clip-from)))

(defmethod redo ((self cmd-clips-d&d-move-from-arrangement-to-sceen-matrix) project)
  (setf (.clips-to self) (with-serialize-context ()
                           (deserialize (.clips-to self))))
  (execute self project))

(defcommand cmd-clips-d&d-move-from-sceen-matrix-to-arrangement (command)
  ((clips-from :initarg :clips-from :accessor .clips-from)
   (clips-to :initform nil :accessor .clips-to)
   (lanes-to :initform nil :accessor .lanes-to)
   (times-to :initform nil :accessor .times-to)))

(defmethod initialize-instance :after ((self cmd-clips-d&d-move-from-sceen-matrix-to-arrangement)
                                       &key clips-to)
  (loop for clip in clips-to
        do (push (.lane clip) (.lanes-to self))
           (push (.time clip) (.times-to self))))

(defmethod execute ((self cmd-clips-d&d-move-from-sceen-matrix-to-arrangement) project)
  (let ((clips-from-serialized (with-serialize-context ()
                                 (serialize (.clips-from self)))))
    (loop for clip-from in (.clips-from self)
          do (clip-delete (.sceen clip-from) clip-from))
    (setf (.clips-from self) clips-from-serialized)))

(defmethod undo ((self cmd-clips-d&d-move-from-sceen-matrix-to-arrangement) project)
  (loop for clip-to in (.clips-to self)
        do (clip-delete (.lane clip-to) clip-to))
  (setf (.clips-from self)
        (loop for clip-from in (with-serialize-context ()
                                 (deserialize (.clips-from self)))
              do (clip-add (.sceen clip-from) clip-from)
              collect clip-from))
  (setf (.clips-to self) nil))

(defmethod redo ((self cmd-clips-d&d-move-from-sceen-matrix-to-arrangement) project)
  (loop for clip-from in (.clips-from self)
        for clip-to = (copy clip-from)
        for time-to in (.times-to self)
        for lane-to in (.lanes-to self)
        do (setf (.time clip-to) time-to)
           (setf (.lane clip-to) lane-to)
           (setf (.sceen clip-to) nil)
           (clip-add lane-to clip-to)
           (push clip-to (.clips-to self)))
  (execute self project))

(defcommand cmd-clips-delete (command)
  ((clips :initarg :clips :accessor .clips)))

(defmethod execute ((self cmd-clips-delete) project)
  (let ((serialized (with-serialize-context () (serialize (.clips self)))))
    (loop for clip in (.clips self)
          for lane = (.lane clip)
          for sceen = (.sceen clip)
          do (clip-delete (or sceen lane) clip)
          collect lane)
    (loop for clip in (.clips self)
          do (terminate clip))
    (setf (.clips self) serialized)))

(defmethod undo ((self cmd-clips-delete) project)
  (setf (.clips self) (with-serialize-context () (deserialize (.clips self))))
  (loop for clip in (.clips self)
        for lane = (.lane clip)
        for sceen = (.sceen clip)
        if sceen
          do (clip-add sceen clip :lane lane)
        else
          do (clip-add lane clip)))

(defcommand cmd-clips-start-change (command)
  ((clips :initarg :clips :accessor .clips)
   (delta :initarg :delta :accessor .delta)
   (stretch-p :initarg :stretch-p :accessor .stretch-p)))

(defmethod execute ((self cmd-clips-start-change) project)
  (loop with delta = (.delta self)
        for clip in (.clips self)
        do (decf (.time clip) delta)
        if (.stretch-p self)
          do (stretch clip (+ (.duration clip) delta))
        else
          do (incf (.duration clip) delta)))

(defmethod undo ((self cmd-clips-start-change) project)
  (loop with delta = (.delta self)
        for clip in (.clips self)
        do (incf (.time clip) delta)
        if (.stretch-p self)
          do (stretch clip (- (.duration clip) delta))
        else
          do (decf (.duration clip) delta)))

(defcommand cmd-clips-end-change (command)
  ((clips :initarg :clips :accessor .clips)
   (delta :initarg :delta :accessor .delta)
   (stretch-p :initarg :stretch-p :accessor .stretch-p)))

(defmethod execute ((self cmd-clips-end-change) project)
  (loop with delta = (.delta self)
        for clip in (.clips self)
        if (.stretch-p self)
          do (stretch clip (+ (.duration clip) delta))
        else
          do (incf (.duration clip) delta)))

(defmethod undo ((self cmd-clips-end-change) project)
  (loop with delta = (.delta self)
        for clip in (.clips self)
        if (.stretch-p self)
          do (stretch clip (- (.duration clip) delta))
        else
          do (decf (.duration clip) delta)))

(defcommand cmd-latency-compute (command)
  ()
  (:default-initargs :undo-p nil))

(defmethod execute ((self cmd-latency-compute) project)
  (latency-compute project))

(defcommand cmd-module-add (command)
  ((track-id :initarg :track-id :accessor .track-id)
   (plugin-info :initarg :plugin-info :accessor .plugin-info)
   (before :initarg :before :initform nil :accessor .before)
   (module :initform nil :accessor .module)))

(defmethod execute ((self cmd-module-add) project)
  (let ((track (find-track project (.track-id self)))
        (module (plugin-load (.plugin-info self))))
    (setf (.module self) module)
    (module-add track module :before (.before self))))

(defmethod undo ((self cmd-module-add) project)
  (let ((track (find-track project (.track-id self)))
        (module (.module self)))
    (when (and track module)
      (module-delete  track module))))

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



(defcommand cmd-range-d&d (command)
  ((clip :initarg :clip :accessor .clip)
   (range-src :initarg :range-src :accessor .range-src)
   (range-dst :initarg :range-dst :accessor .range-dst)
   (notes :accessor .notes)))

(defcommand cmd-range-d&d-copy (cmd-range-d&d)
  ())

(defmethod execute ((self cmd-range-d&d) project)
  (let ((notes (range-copy (.clip self) (.range-src self))))
    (loop for note in notes
          with (time-delta key-delta)
            = (mapcar #'- (.range-dst self) (.range-src self))
          do (incf (.time note) time-delta)
             (incf (.key note) key-delta)
             (note-add (.clip self) note))
    (setf (.notes self) notes)))

(defmethod undo ((self cmd-range-d&d) project)
  (loop for note in (.notes self)
        do (note-delete (.clip self) note)))

(defcommand cmd-range-d&d-move (cmd-range-d&d)
  ((notes-src-serialized :accessor .notes-src-serialized)))

(defmethod execute ((self cmd-range-d&d-move) project)
  ;; TODO
  (let* ((notes-src (loop for note in (.notes (.clips self))
                          if (in-p note (.range-src self))
                            collect note))
         (notes-src-serialized (with-serialize-context ()
                                 (serialize notes-src))))
    (loop for note in notes-src
          do (note-delete (or (.sceen note) (.lane note)) note))
    (setf (.notes-src-serialized self) notes-src-serialized))
  (call-next-method))

(defmethod undo ((self cmd-range-d&d-move) project)
  (call-next-method)
  (loop for note in (with-serialize-context ()
                      (deserialize (.notes-src-serialized self)))
        do (note-add (or (.sceen note) (.lane note)) note)))

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
        (track-new (make-instance 'track :color (color-random)))
        (track-parent (find-neko (.track-id-parent self))))
    (setf (.track-id-new self) (.neko-id track-new))
    (track-add track-parent track-new :before track-before)))

(defmethod undo ((self cmd-track-add) project)
  (let ((track-new (find-neko (.track-id-new self)))
        (track-parent (find-neko (.track-id-parent self))))
    (track-delete track-parent track-new)))

(defcommand cmd-tracks-dd-copy (command)
  ((tracks :initarg :tracks :accessor .tracks)
   (before :initarg :before :accessor .before)
   (tracks-copied :accessor .tracks-copied)))

(defmethod execute ((self cmd-tracks-dd-copy) project)
  (setf (.tracks-copied self)
        (loop for track in (.tracks self)
              collect (copy track)))
  (loop with before = (.before self)
        with parent = (.parent before)
        for track in (reverse (.tracks-copied self))
        do (track-add parent track :before before)))

(defmethod undo ((self cmd-tracks-dd-copy) project)
  (loop with parent = (.parent (.before self))
        for track in (.tracks-copied self)
        do (track-delete parent track)))

(defcommand cmd-tracks-dd-move (command)
  ((tracks :initarg :tracks :accessor .tracks)
   (before :initarg :before :accessor .before)
   (befores :accessor .befores)))

(defmethod execute ((self cmd-tracks-dd-move) project)
  (setf (.befores self) (mapcar #'after (.tracks self)))
  (loop with before = (.before self)
        with parent = (.parent before)
        for track in (reverse (.tracks self))
        do (track-delete (.parent track) track)
           (track-add parent track :before before)))

(defmethod undo ((self cmd-tracks-dd-move) project)
  (loop for track in (.tracks self)
        for before in (.befores self)
        do (track-delete (.parent track) track)
           (track-add (.parent before) track :before before)))

(defcommand cmd-tracks-group (command)
  ((tracks :initarg :tracks :accessor .tracks)
   (track-group :accessor .track-group)
   (parents :accessor .parents)
   (tracks-before :accessor .tracks-before)))

(defmethod execute ((self cmd-tracks-group) project)
  (let* ((track-group (make-instance 'track :name (name-new 'track "GRP")))
         (parents (mapcar #'.parent (.tracks self))))
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
    (track-delete (.parent track-group) track-group)
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

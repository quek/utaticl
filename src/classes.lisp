(in-package :dgw)

(defclass neko ()
  ((neko-id :initarg :neko-id :initform (uid) :accessor .neko-id)
   (name :initarg :name :initform "(noname)" :accessor .name)
   (color :initarg :color :initform (color #x80 #x80 #x80 #x80) :accessor .color)))

(defserialize neko neko-id name color)

(defmethod print-object ((self neko) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a"
            (.name self)
            (.neko-id self))))

(defclass project (neko)
  ((arrangement :initform (make-instance 'arrangement) :accessor .arrangement)
   (piano-roll :initform nil :accessor .piano-roll)
   (commander :initform (make-instance 'commander) :accessor .commander)
   (rack :initform (make-instance 'rack) :accessor .rack)
   (bpm :initform 128.0 :accessor .bpm)
   (cmd-queue :initform nil :accessor .cmd-queue)
   (cmd-undo-stack :initform nil :accessor .cmd-undo-stack)
   (cmd-redo-stack :initform nil :accessor .cmd-redo-stack)
   (master-track :initform (make-instance 'master-track) :accessor .master-track)
   (playing-p :initform nil :accessor .playing-p)
   (transposer :initform (make-instance 'transposer) :accessor .transposer)
   (target-track :initform :nil :accessor .target-track)))


(defclass show-mixin ()
  ((show-p :initarg :show-p :initform nil :accessor .show-p)))

(defclass view ()
  ())

(defclass transposer (view)
  ())

(defclass arrangement (view)
  ((default-lane-height :allocation :class :initform 50.0 :accessor .default-lane-height)
   (lane-height-map :initform (make-hash-table) :accessor .lane-height-map)
   (track-width :initform 150.0 :accessor .track-width)
   (time-ruler-height :initform 20.0 :accessor .time-ruler-height)
   (zoom-x :initform 25.0 :accessor .zoom-x)
   (zoom-y :initform 50.0 :accessor .zoom-y)))

(defclass piano-roll (view)
  ((clip :initarg :clip :accessor .clip)))

(defclass rack (view)
  ((plugin-selector :initform (make-instance 'plugin-selector)
                    :accessor .plugin-selector)) )

(defclass plugin-selector ()
  ((plugin-infos :accessor .plugin-infos)
   (query :initform "" :accessor .query)))

(defclass track (neko)
  ((lanes :initarg :lanes :initform (list (make-instance 'lane)) :accessor .lanes)
   (event-in :accessor .event-in)
   (modules :initform nil :accessor .modules)
   (nbus-audio-in :initform 1 :accessor .nbus-audio-in)
   (nbus-audio-out :initform 1 :accessor .nbus-audio-out)
   (nbus-event-in :initform 1 :accessor .nbus-event-in)
   (nbus-event-out :initform 1 :accessor .nbus-event-out)
   (process-data :accessor .process-data)
   (select-p :initform nil :accessor .select-p)
   (tracks :initform nil :accessor .tracks))
  (:default-initargs :name "TRACK" :color (color #x33 #x33 #x33)))

(defclass master-track (track)
  ()
  (:default-initargs :name "MASTER"))

(defclass lane (neko)
  ((clips :initarg :clips :initform nil :accessor .clips)))

(defclass time-thing (neko)
  ((time :initarg :time :initform 0.0d0 :accessor .time)
   (duration :initarg :duration :initform 16.0d0 :accessor .duration)))

(defclass note (time-thing)
  ((key :initarg :key :initform +c4+ :accessor .key))
  (:default-initargs :duration 1.0d0))

(defmethod print-object ((self note) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d ~d ~d"
            (.key self)
            (.time self)
            (.duration self))))

(defclass clip (time-thing)
  ()
  (:default-initargs :name "CLIP"))

(defclass clip-note (clip)
  ((notes :initform (list (make-instance 'note :key +c4+ :time 0.0d0 :duration 1.0d0)
                          (make-instance 'note :key +e4+ :time 1.0d0 :duration 1.0d0)
                          (make-instance 'note :key +g4+ :time 2.0d0 :duration 1.0d0)
                          (make-instance 'note :key +c5+ :time 3.0d0 :duration 1.0d0))
          :accessor .notes))
  (:default-initargs :color (color #x00 #x80 #x80 #x80)))

(defclass sequence-note (time-thing)
  ())

(defclass plugin-info ()
  ((id :initarg :id :accessor .id)
   (name :initarg :name :accessor .name)
   (path :initarg :path :accessor .path)
   (file-write-date :initarg :file-write-date :accessor .file-write-date)))

(defserialize plugin-info id name path file-write-date)

(defclass plugin-info-vst3 (plugin-info)
  ())

(defclass module (neko)
  ((start-p :initform nil :accessor .start-p)
   (editor-open-p :initform nil :accessor .editor-open-p)))

(defclass vst3-module (module)
  ((library :initarg :library :accessor .library)
   (host-applicaiton :reader .host-applicaiton)
   (factory :initarg :factory :reader .factory)
   (component :initarg :conponent :initform nil :reader .component)
   (controller :initarg :controller :initform nil :reader .controller)
   (single-component-p :initarg :single-component-p :reader .single-component-p)
   (audio-processor :accessor .audio-processor)
   (audio-input-bus-count :accessor .audio-input-bus-count)
   (audio-output-bus-count :accessor .audio-output-bus-count)
   (event-input-bus-count :accessor .event-input-bus-count)
   (event-output-bus-count :accessor .event-output-bus-count)
   (view :initform :nil :accessor .view)
   (hwnd :initform :nil :accessor .hwnd)
   (connection-component :initform nil :accessor .connection-component)
   (connection-controller :initform nil :accessor .connection-controller)
   (parameter-changes-in :initform (make-instance 'vst3-impl::parameter-changes)
                         :accessor .parameter-changes-in)))

(defclass commander (show-mixin)
  ((query :initform "" :accessor .query)))

(defclass app ()
  ((mutex :initform (sb-thread:make-mutex :name "APP") :accessor .mutex)
   (projects :initform (list (make-instance 'project)) :accessor .projects)))

(in-package :dgw)

(defclass neko ()
  ((name :initarg :name :initform "" :accessor .name)
   (color :initarg :color :initform nil :accessor .color)))

(defclass project (neko)
  ((arrangement :initform (make-instance 'arrangement) :accessor .arrangement)
   (commander :initform (make-instance 'commander) :accessor .commander)
   (rack :initform (make-instance 'rack) :accessor .rack)
   (bpm :initform 128.0 :accessor .bpm)
   (cmd-queue :initform nil :accessor .cmd-queue)
   (cmd-undo-stack :initform nil :accessor .cmd-undo-stack)
   (cmd-redo-stack :initform nil :accessor .cmd-redo-stack)
   (master-track :initform (make-instance 'master-track) :accessor .master-track)
   (playing-p :initform nil :accessor .playing-p)
   (transposer :initform (make-instance 'transposer) :accessor .transposer)
   ;;TODO DELETE
   (module :initform nil :accessor .module)))

(defclass transposer ()
  ())

(defclass arrangement ()
  ((track-width :initform 150.0 :accessor .track-width)
   (zoom-x :initform 50.0 :accessor .zoom-x)
   (zoom-y :initform 50.0 :accessor .zoom-y)))

(defclass rack ()
  ())

(defclass track (neko)
  ((clips :initarg :clips :initform nil :accessor .clips)
   (modules :initform nil :accessor .modules)
   (select-p :initform nil :accessor .select-p)
   (tracks :initform nil :accessor .tracks))
  (:default-initargs :name "TRACK" :color (color #x33 #x33 #x33)
                     :clips (list (make-instance 'clip-note)) ;TODO DELETE
   ))

(defclass master-track (track)
  ()
  (:default-initargs :name "MASTER"))

(defclass time-thing (neko)
  ((time :initarg :time :initform 0.0d0 :accessor .time)
   (duration :initarg :duration :initform 4.0d0 :accessor .duration)))

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
  ())

(defclass clip-note (clip)
  ((notes :initform (list (make-instance 'note :key +c4+ :time 0.0d0 :duration 1.0d0)
                          (make-instance 'note :key +e4+ :time 1.0d0 :duration 1.0d0)
                          (make-instance 'note :key +g4+ :time 2.0d0 :duration 1.0d0)
                          (make-instance 'note :key +c5+ :time 3.0d0 :duration 1.0d0))
          :accessor .notes)))

(defclass piano-roll ()
  ())

(defclass commander ()
  ((show-p :initarg :show-p :initform nil :accessor .show-p)))

(defclass app ()
  ((projects :initform (list (make-instance 'project)) :accessor .projects)))

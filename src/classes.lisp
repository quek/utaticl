(in-package :dgw)

(defclass neko ()
  ((name :initarg :name :initform "" :accessor .name)
   (color :initarg :color :initform nil :accessor .color)))

(defclass project (neko)
  ((arrangement :initform (make-instance 'arrangement) :accessor .arrangement)
   (bpm :initform 128.0 :accessor .bpm)
   (cmd-queue :initform nil :accessor .cmd-queue)
   (cmd-undo-stack :initform nil :accessor .cmd-undo-stack)
   (cmd-redo-stack :initform nil :accessor .cmd-redo-stack)
   (master-track :initform (make-instance 'master-track) :accessor .master-track)
   (playing-p :initform nil :accessor .playing-p)
   (transposer :initform (make-instance 'transposer) :accessor .transposer)
   ;;TODO DELETE
   (module :initform nil :accessor .module)))

(defclass transposer (neko)
  ())

(defclass arrangement (neko)
  ((track-width :initform 150.0 :accessor .track-width)
   (zoom-x :initform 50.0 :accessor .zoom-x)
   (zoom-y :initform 50.0 :accessor .zoom-y)))

(defclass track (neko)
  ((clips :initform nil :accessor .clips)
   (modules :initform nil :accessor .modules)
   (select-p :initform nil :accessor .select-p)
   (tracks :initform nil :accessor .tracks))
  (:default-initargs :name "TRACK" :color (color #x33 #x33 #x33)))

(defclass master-track (track)
  ()
  (:default-initargs :name "MASTER"))

(defclass piano-roll (neko)
  ())

(defclass app ()
  ((projects :initform (list (make-instance 'project)) :accessor .projects)))

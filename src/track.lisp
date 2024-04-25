(in-package :dgw)

(defclass track (neko)
  ((clips :initform nil :accessor .clips)
   (modules :initform nil :accessor .modules)
   (tracks :initform nil :accessor tracks))
  (:default-initargs :name "TRACK"))

(defclass master-track (track)
  ()
  (:default-initargs :name "MASTER"))

(defmethod render ((self track) context)
  (ig:push-id-int (sxhash self))
  (ig:button (.name self))
  (ig:pop-id))

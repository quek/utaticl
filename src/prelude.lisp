(in-package :dgw)

(log:config :trace)

(defparameter *working-directory* (asdf:system-source-directory :dgw))

(sb-ext:defglobal *app* nil)
(sb-ext:defglobal *audio* nil)

(defvar *project*)


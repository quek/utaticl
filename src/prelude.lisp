(in-package :dgw)

(log:config :trace)

(defparameter *working-directory* (asdf:system-source-directory :dgw))

(sb-ext:defglobal *thread-pool* nil)
(sb-ext:defglobal *app* nil)
(sb-ext:defglobal *hwnd* nil)
(sb-ext:defglobal *audio* nil)
(sb-ext:defglobal *done* nil)
;;(setf *done* t)
(defparameter *invoke-debugger-p* t)


(defvar *project*)
(defvar *process-data*)

(in-package :utaticl.core)

(log:config :trace)

(defparameter *working-directory* (asdf:system-source-directory :utaticl))

(sb-ext:defglobal *thread-pool* nil)
(sb-ext:defglobal *app* nil)
(sb-ext:defglobal *hwnd* nil)
(sb-ext:defglobal *done* nil)
(defparameter *invoke-debugger-p* t)

(defvar *mouse-pos*)
(defvar *project*)
(defvar *process-data*)

(defconstant +side-threshold+ 5.0)


(defparameter *default-lane-width* 60.0)

(in-package :utaticl.core)

(log:config :trace)

(defparameter *working-directory* (asdf:system-source-directory :utaticl))

(sb-ext:defglobal *thread-pool* nil)
(sb-ext:defglobal *app* nil)
(sb-ext:defglobal *hwnd* nil)
(sb-ext:defglobal *done* nil)
(defparameter *invoke-debugger-p* t)

(defvar *mouse-pos*)
(defvar *draw-list*)
(defvar *style*)
(defvar *scrollbar-size*)
(defvar *item-spacing-x*)
(defvar *item-spacing-y*)

(defvar *project*)
(defvar *process-data*)

(defconstant +side-threshold+ 5.0)
(defparameter *text-margin* 4.0)

(defparameter *default-lane-width* 60.0)

(defparameter *osc-port-audio* 53077)
(defparameter *osc-port-gui*   53088)

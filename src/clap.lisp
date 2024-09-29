(defpackage :utaticl.clap
  (:use :cl :anaphora :utaticl.core))

(in-package :utaticl.clap)

(defvar *object-map* (make-hash-table :weakness :value))

(defmethod object-get ((pointer sb-sys:system-area-pointer))
  (gethash (cffi:pointer-address pointer) *object-map*))

(defmethod object-get ((wrap structure-object))
  (object-get (autowrap:ptr wrap)))

(defmethod object-insert ((pointer sb-sys:system-area-pointer) object)
  (setf (gethash (cffi:pointer-address pointer) *object-map*)
        object))

(defmethod object-insert ((wrap structure-object) object)
  (object-insert (autowrap:ptr wrap) object))

(cffi:defcallback gui.resize-hints-changed :void
    ((host :pointer))
  (log:trace "gui.resize-hints-changed" host)
  (resize-hints-changed (gethash (cffi:pointer-address host) *object-map*)))

(cffi:defcallback gui.request-resize :bool
    ((host :pointer)
     (width :unsigned-int)
     (height :unsigned-int))
  (log:trace "gui.request-resize" host)
  (request-resize (gethash (cffi:pointer-address host) *object-map*)
                  width height))

(cffi:defcallback gui.request-show :bool
    ((host :pointer))
  (log:trace "gui.request-show" host)
  (editor-open (gethash (cffi:pointer-address host) *object-map*))
  t)

(cffi:defcallback gui.request-hide :bool
    ((host :pointer))
  (log:trace "gui.request-hide" host)
  (editor-close (gethash (cffi:pointer-address host) *object-map*))
  t)

(cffi:defcallback gui.closed :void
    ((host :pointer)
     (was-destroyed :bool))
  (log:trace "gui.request-closed" host)
  (closed (gethash (cffi:pointer-address host) *object-map*) was-destroyed))

(cffi:defcallback host.get-extension :pointer
    ((host :pointer)
     (extension-id :string))
  (log:trace "host.get-extension" host extension-id)
  (let ((module-clap (gethash (cffi:pointer-address host) *object-map*)))
    (cond ((equal extension-id "clap.gui")
           (autowrap:ptr (.clap-host-gui module-clap)))
          (t (cffi:null-pointer)))))

(cffi:defcallback host.request-restart :void
    ((host :pointer))
  (log:trace "host.request-restart" host))

(cffi:defcallback host.request-process :void
    ((host :pointer))
  (log:trace "host.request-process" host))

(cffi:defcallback host.request-callback :void
    ((host :pointer))
  (log:trace "host.request-callback" host))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %def-clap-method (method class args return-type body)
    (let ((this (gensym "SELF"))
          (method (intern (format nil "~a.~a" class method))))
      `(progn
         (defmethod ,method ((self ,class) ,@(mapcar #'car args))
           ,@body)
         (cffi:defcallback ,method ,return-type
             ((,this :pointer)
              ,@args)
           (,method (object-get ,this) ,@(mapcar #'car args))))))

  (defun %set-clap-struct-callback (struct field)
    (let* ((writer (intern (format nil "CLAP-~a.~a" struct field) :clap))
           (callback (intern (format nil "~a.~a" struct field))))
      `(setf (,writer it)
             (cffi:callback ,callback)))))

(defmacro def-clap-struct (name slots methods)
  `(progn
     (defstruct (,name (:include ,(intern (format nil "CLAP-~a-T" name) :clap))
                       (:constructor ,(intern
                                       (format nil "%MAKE-~a" name))))
       ,@slots)
     (defun ,(intern (format nil "MAKE-~a" name)) (&key ptr ,@slots)
       (anaphora:aprog1
           (,(intern (format nil "%MAKE-~a" name))
            :ptr (or ptr
                     (autowrap:ptr
                      (aprog1
                          (autowrap:calloc
                           ',(intern (format nil "CLAP-~a-T" name) :clap))
                        ,@(loop for (method-name) in methods
                                collect (%set-clap-struct-callback name method-name)))))
            ,@(loop for (arg) in slots
                    nconc `(,(intern (symbol-name arg) :keyword) ,arg)))
         (object-insert (autowrap:ptr it) it)
         (initialize it)))
     ,@ (loop for (method-name args return-type . body) in methods
              collect (%def-clap-method method-name name args return-type body))))

(defgeneric initialize (clap-struct)
  (:method (x)))

(def-clap-struct event-transport () ())

(def-clap-struct audio-buffer
    ((nbuses 0))
  ())

(defmethod initialize ((self audio-buffer))
  (let ((nbuses (audio-buffer-nbuses self)))
    (setf (audio-buffer-ptr self)
          (autowrap:ptr (autowrap:calloc 'clap:clap-audio-buffer-t nbuses)))
    (loop for bus below nbuses
          for x = (autowrap:c-aref self bus 'clap:clap-audio-buffer-t)
          with nchannels = 2
          do (setf (clap:clap-audio-buffer.channel-count x) nchannels)
             (setf (clap:clap-audio-buffer.data32 x)
                   (autowrap:calloc :pointer nchannels)))))

(defmethod apply-from ((self audio-buffer) (audio-bus audio-bus) &key bus)
  (loop with clap-audio-buffer = (autowrap:c-aref self bus 'clap:clap-audio-buffer-t)
        for channel below (.nchannels audio-bus)
        do (setf (cffi:mem-aref (clap:clap-audio-buffer.data32 clap-audio-buffer)
                                :pointer channel)
                 (buffer-at audio-bus channel))))

(defmethod bus-at ((self audio-buffer) bus)
  (autowrap:c-aref self bus 'clap:clap-audio-buffer-t))

(defmethod terminate ((self audio-buffer) &key)
  (loop for bus below (audio-buffer-nbuses self)
        for x = (autowrap:c-aref self bus 'clap:clap-audio-buffer-t)
        do (autowrap:free (clap:clap-audio-buffer.data32 x))))

(def-clap-struct input-events
    ((list (make-array 16 :fill-pointer 0)))
  ((size () :unsigned-int
         (length (input-events-list self)))
   (get ((index :unsigned-int)) :pointer
        (autowrap:ptr (aref (input-events-list self) index)))))

(defmethod terminate ((self input-events) &key)
  (loop for event across (input-events-list self)
        do (terminate event)))

(defmethod event-add ((self input-events) event note sample-offset)
  (let ((clap-event-note (autowrap:calloc 'clap:clap-event-note-t)))
    (setf (clap:clap-event-note.header.size clap-event-note)
          (autowrap:sizeof 'clap:clap-event-note-t))
    (setf (clap:clap-event-note.header.time clap-event-note)
          sample-offset)
    (setf (clap:clap-event-note.header.space-id clap-event-note)
          clap:+clap-core-event-space-id+)
    (setf (clap:clap-event-note.header.type clap-event-note)
          (ecase event
            (:on clap:+clap-event-note-on+)
            (:off clap:+clap-event-note-off+)))
    (setf (clap:clap-event-note.header.flags clap-event-note)
          0)

    (setf (clap:clap-event-note.note-id clap-event-note)
          -1)
    (setf (clap:clap-event-note.port-index clap-event-note)
          -1)
    (setf (clap:clap-event-note.channel clap-event-note)
          (.channel note))
    (setf (clap:clap-event-note.key clap-event-note)
          (.key note))
    (setf (clap:clap-event-note.velocity clap-event-note)
          (ecase event
            (:on (.velocity note))
            (:off 1.0d0)))

    (vector-push-extend clap-event-note (input-events-list self))))

(defmethod prepare ((self input-events))
  (loop for x across (input-events-list self)
        do (autowrap:free x))
  (setf (fill-pointer (input-events-list self)) 0))

(def-clap-struct output-events
    ((list (make-array 16 :fill-pointer 0)))
  ((try-push ((event :pointer)) ;const clap_event_header_t *event
             :bool
             (vector-push-extend (clap::make-clap-event-header-t :ptr event)
                                 (output-events-list self))
             t)))

(defmethod prepare ((self output-events))
  (setf (fill-pointer (output-events-list self)) 0))

(def-clap-struct process
    ((transport (make-event-transport))
     (audio-inputs (make-audio-buffer :ptr (cffi:null-pointer)))
     (audio-outputs (make-audio-buffer :ptr (cffi:null-pointer)))
     (input-events (make-input-events))
     (output-events (make-output-events)))
  ())

(defmethod initialize ((self process))
  (pointer-set self))

(defmethod terminate ((self process) &key)
  (terminate (process-transport self))
  (terminate (process-audio-inputs self))
  (terminate (process-audio-outputs self))
  (terminate (process-input-events self))
  (terminate (process-output-events self)))

(defmethod apply-from :before ((self process) (process-data process-data) &key)
  (let ((nbuses-input (length (.inputs process-data)))
        (audio-inputs (process-audio-inputs self))
        (nbuses-output (length (.outputs process-data)))
        (audio-outputs (process-audio-outputs self)))
    (assert (= nbuses-input nbuses-output)
            (nbuses-input nbuses-output)
            "swap-in-out するので同じはず")
    (setf (clap:clap-process.audio-inputs-count self)
          nbuses-input)
    (setf (clap:clap-process.audio-outputs-count self)
          nbuses-output)
    (when (/= nbuses-input (audio-buffer-nbuses audio-inputs))
      (terminate audio-inputs)
      (setf (process-audio-inputs self)
            (make-audio-buffer :nbuses nbuses-input))
      (pointer-set self))
    (when (/= nbuses-output (audio-buffer-nbuses audio-outputs))
      (terminate audio-outputs)
      (setf (process-audio-outputs self)
            (make-audio-buffer :nbuses nbuses-output))
      (pointer-set self))))

(defmethod apply-from ((self process) (process-data process-data) &key)
  (loop for input in (.inputs process-data)
        for bus below (clap:clap-process.audio-inputs-count self)
        do (apply-from (process-audio-inputs self) input :bus bus))
  (loop for output in (.outputs process-data)
        for bus below (clap:clap-process.audio-outputs-count self)
        do (apply-from (process-audio-outputs self) output :bus bus))

  (loop with from-events = (.input-events process-data)
        with input-events = (process-input-events self)
        for event across (.events from-events)
        for note across (.notes from-events)
        for sample-offset across (.sample-offsets from-events)
        do (event-add input-events event note sample-offset)))

(defmethod prepare ((self process))
  (prepare (process-input-events self))
  (prepare (process-output-events self)))

(defmethod pointer-set ((self process))
  (setf (clap:clap-process.transport self)
        (autowrap:ptr (process-transport self)))
  (setf (clap:clap-process.audio-inputs self)
        (autowrap:ptr (process-audio-inputs self)))
  (setf (clap:clap-process.audio-outputs self)
        (autowrap:ptr (process-audio-outputs self)))
  (setf (clap:clap-process.in-events self)
        (autowrap:ptr (process-input-events self)))
  (setf (clap:clap-process.out-events self)
        (autowrap:ptr (process-output-events self))))


(alexandria:define-constant +host-name+ "UTATICL" :test 'equal)
(alexandria:define-constant +host-url+ "https://github.com/quek/utaticl" :test 'equal)
(alexandria:define-constant +host-version+ "0.0.1" :test 'equal)
(defvar +window-api-win32+ (cffi:foreign-string-alloc "win32"))


(defmacro call (function &rest args-and-return-type)
  `(cffi:foreign-funcall-pointer
    ,function
    ()
    :pointer (autowrap:ptr (utaticl.core::.plugin self))
    ,@args-and-return-type))

(defun get-factory (path)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
    (let* ((lib (cffi:foreign-funcall "LoadLibraryA" :string path :pointer))
           (clap-entry (cffi:foreign-funcall "GetProcAddress" :pointer lib :string "clap_entry" :pointer))
           (clap-entry (clap::make-clap-plugin-entry :ptr clap-entry))
           (init (clap:clap-plugin-entry.init clap-entry)))
      (cffi:foreign-funcall-pointer init ()
                                    :string path
                                    :bool)
      (let* ((factory (cffi:foreign-funcall-pointer
                       (clap:clap-plugin-entry.get-factory clap-entry) ()
                       :string "clap.plugin-factory"
                       :pointer))
             (factory (clap::make-clap-plugin-factory :ptr factory)))
        (values factory lib)))))

;(get-factory "c:/Program Files/Common Files/CLAP/Surge Synth Team/Surge XT.clap")

(defun create-plugin (factory id)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
    (let ((host (autowrap:alloc 'clap:clap-host-t)))
      (setf (clap:clap-host.clap-version.major host) 1
            (clap:clap-host.clap-version.minor host) 1
            (clap:clap-host.clap-version.revision host) 10)
      (setf (clap:clap-host.name host) (sb-sys:vector-sap +host-name+))
      (setf (clap:clap-host.vendor host) (sb-sys:vector-sap +host-name+))
      (setf (clap:clap-host.url host) (sb-sys:vector-sap +host-url+))
      (setf (clap:clap-host.version host) (sb-sys:vector-sap +host-version+))
      (setf (clap:clap-host.get-extension host) (cffi:callback host.get-extension))
      (setf (clap:clap-host.request-restart host) (cffi:callback host.request-restart))
      (setf (clap:clap-host.request-process host) (cffi:callback host.request-process))
      (setf (clap:clap-host.request-callback host) (cffi:callback host.request-callback))

      (let* ((plugin (cffi:foreign-funcall-pointer
                      (clap:clap-plugin-factory.create-plugin factory) ()
                      :pointer (autowrap:ptr factory)
                      :pointer (autowrap:ptr host)
                      :string id
                      :pointer)))
        (values (clap::make-clap-plugin :ptr plugin) host)))))

(defun prepare-callbacks (module-clap)
  (let ((clap-host-gui (.clap-host-gui module-clap)))
    (setf (clap:clap-host-gui.resize-hints-changed clap-host-gui)
          (cffi:callback gui.resize-hints-changed))
    (setf (clap:clap-host-gui.request-resize clap-host-gui)
          (cffi:callback gui.request-resize))
    (setf (clap:clap-host-gui.request-show clap-host-gui)
          (cffi:callback gui.request-show))
    (setf (clap:clap-host-gui.request-hide clap-host-gui)
          (cffi:callback gui.request-hide))
    (setf (clap:clap-host-gui.closed clap-host-gui)
          (cffi:callback gui.closed))))

(defun query-audio-ports (self)
  (let ((ext (.ext-audio-ports self)))
    (if ext
        (progn
          (setf (.audio-input-bus-count self)
                (call (clap:clap-plugin-audio-ports.count ext)
                                    :bool t
                                    :unsigned-int))
          (setf (.audio-output-bus-count self)
                (call (clap:clap-plugin-audio-ports.count ext)
                                    :bool nil
                                    :unsigned-int)))
        (progn
          (setf (.audio-input-bus-count self) 0)
          (setf (.audio-output-bus-count self) 0)))))

(defun query-note-ports (self)
  (let ((ext (.ext-note-ports self)))
    (if ext
        (progn
          (setf (.event-input-bus-count self)
                (call (clap:clap-plugin-note-ports.count ext)
                                    :bool t
                                    :unsigned-int))
          (setf (.event-output-bus-count self)
                (call (clap:clap-plugin-note-ports.count ext)
                                    :bool nil
                                    :unsigned-int)))
        (progn
          (setf (.event-input-bus-count self) 0)
          (setf (.event-output-bus-count self) 0)))))


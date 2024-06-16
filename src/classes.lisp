(in-package :dgw)

(defclass neko ()
  ((neko-id :initarg :neko-id :initform (uid) :accessor .neko-id)
   (name :initarg :name :initform "" :accessor .name)
   (color :initarg :color :initform (color #x80 #x80 #x80 #x80) :accessor .color)))

(defserialize neko neko-id name color)

(defmethod print-object ((self neko) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a"
            (.name self)
            (.neko-id self))))

(defclass project (neko)
  ((arrangement :initform (make-instance 'arrangement) :accessor .arrangement)
   (dirty-p :initform nil :accessor .dirty-p)
   (piano-roll :initform nil :accessor .piano-roll)
   (commander :initform (make-instance 'commander) :accessor .commander)
   (rack :initform (make-instance 'rack) :accessor .rack)
   (bpm :accessor .bpm)
   (sec-per-beat :accessor .sec-per-beat)
   (samples-per-beat :accessor .samples-per-beat)
   (cmd-queue :initform nil :accessor .cmd-queue)
   (cmd-undo-stack :initform nil :accessor .cmd-undo-stack)
   (cmd-redo-stack :initform nil :accessor .cmd-redo-stack)
   (mailbox :initform (sb-concurrency:make-mailbox) :accessor .mailbox)
   (master-track :initform (make-instance 'master-track) :accessor .master-track)
   (path :initform nil :accessor .path)
   (play-p :initform nil :accessor .play-p)
   (play-just-stop-p :initform nil :accessor .play-just-stop-p)
   (play-start :initarg :play-start :initform .0d0 :accessor .play-start)
   (play-end :initarg :play-end :initform .0d0 :accessor .play-end)
   (loop-start :initarg :loop-start :initform .0d0 :accessor .loop-start)
   (loop-end :initarg :loop-end :initform 16.0d0 :accessor .loop-end)
   (loop-p :initarg :loop-p :initform t :accessor .loop-p)
   (transposer :initform (make-instance 'transposer) :accessor .transposer)
   (target-track :initform :nil :accessor .target-track)))

(defserialize project bpm master-track loop-start loop-end loop-p)

(defclass show-mixin ()
  ((show-p :initarg :show-p :initform nil :accessor .show-p)))

(defclass time-ruler-mixin ()
  ((time-ruler-threshold :initform 25.0 :accessor .time-ruler-threshold)))

(defclass zoom-mixin ()
  ((zoom-x :initarg :zoom-x :initform 25.0 :accessor .zoom-x)
   (zoom-x-factor :initarg :zoom-x-factor :initform .5 :accessor .zoom-x-factor)
   (zoom-x-min :initarg :zoom-x-min :initform .1 :accessor .zoom-x-min)
   (zoom-y :initarg :zoom-y :initform 50.0 :accessor .zoom-y)
   (zoom-y-factor :initarg :zoom-y-factor :initform .5 :accessor .zoom-y-factor)
   (zoom-y-min :initarg :zoom-y-min :initform .1 :accessor .zoom-y-min)))

(defclass scroll-mixin ()
  ())

(defclass offset-mixin ()
  ())

(defclass grid-mixin ()
  ((grid-snap-p :initarg :grid-snap-p :initform t :accessor .grid-snap-p)
   (grid-unit :initarg :grid-unit :initform +grid-beat+ :accessor .grid-unit)))

(defclass view ()
  ())

(defclass report-window (show-mixin view)
  ((message :initform "" :accessor .message)))

(defclass transposer (view)
  ())

(defclass arrangement (time-ruler-mixin grid-mixin offset-mixin scroll-mixin zoom-mixin view)
  ((clip-at-mouse :initform nil :accessor .clip-at-mouse)
   (clip-drag-offset :initform .0 :accessor .clip-drag-offset)
   (clip-lane-map :initform (make-hash-table) :accessor .clip-lane-map)
   (clip-target :initform nil :accessor .clip-target)
   (clips-selected :initform nil :accessor .clips-selected)
   (clips-dragging :initform nil :accessor .clips-dragging)
   (default-lane-width :allocation :class :initform 50.0 :accessor .default-lane-width)
   (lane-at-mouse :initform nil :accessor .lane-at-mouse)
   (lane-width-map :initform (make-hash-table) :accessor .lane-width-map)
   (offset-y :initform 150.0 :accessor .offset-y)
   (range-selecting-p :initform nil :accessor .range-selecting-p)
   (time-ruler-width :initform 20.0 :accessor .time-ruler-width))
  (:default-initargs :zoom-x 25.0 :zoom-y 50.0 :zoom-x-factor .5 :zoom-y-factor .5
                     :grid-unit +grid-bar+))

(defclass piano-roll (time-ruler-mixin grid-mixin offset-mixin scroll-mixin zoom-mixin view)
  ((clip :initarg :clip :accessor .clip)
   (drag-mode :initform :move :accessor .drag-mode
              :type (member :start :move :end))
   (note-at-mouse :initform nil :accessor .note-at-mouse)
   (note-default-duration :initform 1 :accessor .note-default-duration)
   (note-drag-offset :initform .0 :accessor .note-drag-offset)
   (note-target :initform nil :accessor .note-target)
   (notes-dragging :initform nil :accessor .notes-dragging)
   (notes-dragging-time :initform nil :accessor .notes-dragging-time)
   (notes-dragging-duration :initform nil :accessor .notes-dragging-duration)
   (notes-selected :initform nil :accessor .notes-selected)
   (offset-x :initform 25.0 :accessor .offset-x)
   (offset-y :initform 30.0 :accessor .offset-y)
   (range-selecting-mode :initform nil :accessor .range-selecting-mode
                         :type (member :note :region nil))
   (range-selecting-pos1 :initform nil :accessor .range-selecting-pos1)
   (range-selecting-pos2 :initform nil :accessor .range-selecting-pos2)
   (render-first-p :initform t :accessor .render-first-p)
   (threshold-text-hide :initform 18.0 :accessor .threshold-text-hide))
  (:default-initargs :zoom-x 25.0 :zoom-y 25.0 :zoom-x-factor .5 :zoom-y-factor .5 :zoom-y-min 5.0
                     :grid-unit +grid-beat+))

(defclass rack (view)
  ((plugin-selector :initform (make-instance 'plugin-selector)
                    :accessor .plugin-selector)) )

(defclass plugin-selector (view)
  ((plugin-infos :accessor .plugin-infos)
   (query :initform "" :accessor .query)))

(defclass track (neko)
  ((lanes :initarg :lanes :initform (list (make-instance 'lane)) :accessor .lanes)
   (event-in :accessor .event-in)
   (modules :initform (list (aprog1 (make-instance 'module-gain-track)
                              (start it))
                            (aprog1 (make-instance 'module-fader-track)
                              (start it)))
            :accessor .modules)
   (module-wait-for :initform nil :accessor .module-wait-for)
   (nbus-audio-in :initform 1 :accessor .nbus-audio-in)
   (nbus-audio-out :initform 1 :accessor .nbus-audio-out)
   (nbus-event-in :initform 1 :accessor .nbus-event-in)
   (nbus-event-out :initform 1 :accessor .nbus-event-out)
   (process-data :accessor .process-data)
   (select-p :initform nil :accessor .select-p)
   (tracks :initform nil :accessor .tracks))
  (:default-initargs :name "TRACK" :color (color #x33 #x33 #x33)))

(defserialize track lanes modules tracks)

(defclass master-track (track)
  ()
  (:default-initargs :name "MASTER"))

(defclass lane (neko)
  ((clips :initarg :clips :initform nil :accessor .clips)))

(defserialize lane clips)

(defclass time-thing (neko)
  ((time :initarg :time :initform 0.0d0 :accessor .time)
   (duration :initarg :duration :initform 16.0d0 :accessor .duration)))

(defserialize time-thing time duration)

(defclass note (time-thing)
  ((key :initarg :key :initform +c4+ :accessor .key)
   (channel :initarg :channel :initform 0 :accessor .channel)
   (velocity :initarg :velocity :initform .8 :accessor .velocity))
  (:default-initargs :duration 1.0d0 :color (color #x30 #xc0 #x30 #x80)))

(defserialize note key channel velocity)

(defmethod print-object ((self note) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d ~d ~d"
            (.key self)
            (.time self)
            (.duration self))))

(defclass clip (time-thing)
  ((seq :initarg :seq :accessor .seq)))

(defserialize clip seq)

(defclass clip-note (clip)
  ()
  (:default-initargs :name nil :color nil
                     :seq (make-instance 'seq-note)))

(defserialize clip-note)

(defclass seq-note (time-thing)
  ((notes :initarg :notes :initform nil :accessor .notes))
  (:default-initargs :name "NOTES" :color (color #x30 #xc0 #x30 #x80)))

(defserialize seq-note notes)

(defclass plugin-info ()
  ((id :initarg :id :accessor .id)
   (name :initarg :name :accessor .name)
   (path :initarg :path :accessor .path)
   (file-write-date :initarg :file-write-date :accessor .file-write-date)))

(defserialize plugin-info id name path file-write-date)

(defclass plugin-info-vst3 (plugin-info)
  ())

(defclass module (neko)
  ((id :initarg :id :accessor .id)
   (connections :initform nil :accessor .connections)
   (editor-open-p :initform nil :accessor .editor-open-p)
   (start-p :initform nil :accessor .start-p)
   (params :initform (make-hash-table) :accessor .params)
   (process-done :initform nil :accessor .process-done)))

(defserialize module connections)

(defclass module-vst3 (module)
  ((library :initarg :library :accessor .library)
   (host-applicaiton :reader .host-applicaiton)
   (factory :initarg :factory :accessor .factory)
   (component :initarg :conponent :initform nil :accessor .component)
   (controller :initarg :controller :initform nil :accessor .controller)
   (single-component-p :initarg :single-component-p :accessor .single-component-p)
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

(defclass module-builtin (module)
  ())

(defclass module-fader (module-builtin)
  ()
  (:default-initargs :id 'module-fader :name "Fader"))

(defclass module-track-mixin () ()
  (:documentation "トラック備え付け"))

(defclass module-fader-track (module-track-mixin module-fader)
  ()
  (:documentation "トラック備え付け")
  (:default-initargs :id 'module-fader-track))

(defclass module-gain (module-builtin)
  ()
  (:default-initargs :id 'module-gain :name "Gain"))

(defclass module-gain-track (module-track-mixin module-gain)
  ()
  (:documentation "トラック備え付け")
  (:default-initargs :id 'module-gain-track))

(defclass preset () ())

(defclass preset-vst3 (preset)
  ((buffer :initarg :buffer
           :initform (make-instance 'vst3-impl::bstream)
           :accessor .buffer)))

(defclass connection (neko)
  ((from :initarg :from :accessor .from)
   (to :initarg :to :accessor .to)
   (from-bus-index :initarg :from-bus-index :initform 0 :accessor .from-bus-index)
   (to-bus-index :initarg :to-bus-index :initform 0 :accessor .to-bus-index)
   (from-process-data :accessor .from-process-data)))

(defserialize connection (:ref from) (:ref to) from-bus-index to-bus-index)

(defclass param (neko)
  ((id :initarg :id :initform nil :accessor .id)
   (value :initarg :value :initform .0d0 :accessor .value)))

(defserialize param id value)

(defclass process-data ()
  ((wrap  :accessor .wrap)
   (inputs :accessor .inputs)
   (outputs :accessor .outputs)
   (input-events :accessor .input-events)
   (output-events :accessor .output-events)
   (input-parameter-changes :accessor .input-parameter-changes)
   (output-parameter-changes :accessor .output-parameter-changes)
   (context :accessor .context)
   (notes-on :initform nil :accessor .notes-on)))

(defclass audio-bus-buffers ()
  ((ptr :accessor .ptr)
   (nbuses :initarg :nbuses :initform 1 :accessor .nbuses)))

(defclass audio-device-window (view)
  ((api :initform nil :accessor .api)
   (name :initform nil :accessor .name)
   (host-apis :initform nil :accessor .host-apis)
   (device-infos :initform nil :accessor .device-infos)
   (sample-rate :initform nil :accessor .sample-rate)
   (supported-standard-sample-reates :initform nil
                                     :accessor .supported-standard-sample-reates)))

(defclass commander (show-mixin)
  ((query :initform "" :accessor .query)))

(defclass audio-device ()
  ((device-api :initarg :device-api
               :initform "ASIO"
               :accessor .device-api)
   (device-name :initarg :device-name
                :initform "Prism Sound USB Audio Class 2.0"
                :accessor .device-name)
   (handle :initform (cffi:foreign-alloc :pointer) :accessor .handle)
   (sample-format :initarg sample-format
                  :initform :float
                  :accessor .sample-format)
   (processing :initform nil :accessor .processing)
   (stream :initform nil :accessor .stream)
   (input-channels :initarg :input-channels
                   :initform 0
                   :type fixnum
                   :accessor .input-channels)
   (output-channels :initarg :output-channels
                    :initform 2
                    :type fixnum
                    :accessor .output-channels)
   (master-buffer :initform (list (make-array 1024 :element-type 'single-float :initial-element 0.0)
                                  (make-array 1024 :element-type 'single-float :initial-element 0.0))
                  :accessor .master-buffer)
   (statistic-enter-time :initform (get-internal-real-time)
                         :accessor .statistic-enter-time)
   (statistic-leave-time :initform (get-internal-real-time)
                         :accessor .statistic-leave-time)
   (statistic-count :initform 0 :accessor .statistic-count)
   (statistic-total-process-time :initform 0
                                 :accessor .statistic-total-process-time)
   (statistic-min-process-time :initform most-positive-fixnum
                               :accessor .statistic-min-process-time)
   (statistic-max-process-time :initform 0
                               :accessor .statistic-max-process-time)
   (statistic-total-interval-time :initform 0
                                  :accessor .statistic-total-interval-time)
   (statistic-min-interval-time :initform most-positive-fixnum
                                :accessor .statistic-min-interval-time)
   (statistic-max-interval-time :initform 0
                                :accessor .statistic-max-interval-time)
   (statistic-summary :initform ""
                      :accessor .statistic-summary)))

(defclass app ()
  ((audio-device :initform nil :accessor .audio-device)
   (audio-device-window :initform (make-instance 'audio-device-window)
                        :accessor .audio-device-window)
   (mutex :initform (sb-thread:make-mutex :name "APP") :accessor .mutex)
   (projects :initform nil :accessor .projects)
   (window :initarg :window :accessor .window)))

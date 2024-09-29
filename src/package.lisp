(declaim (optimize (debug 3) (safety 3)))

(defpackage :utaticl
  (:use :cl)
  (:export #:main))

(defpackage :utaticl.core
  (:use :cl :anaphora)
  (:export
   #:*app*
   #:*config*
   #:*dd-at*
   #:*dd-srcs*
   #:*default-lane-width*
   #:*done*
   #:drag-enter
   #:drop
   #:*hwnd*
   #:*hwnd-module-map*
   #:*invoke-debugger-p*
   #:*mouse-pos*
   #:*process-data*
   #:*project*
   #:*render-context*
   #:*thread-pool*
   #:*working-directory*
   #:+side-threshold+
   #:.audio-input-bus-count
   #:.audio-output-bus-count
   #:.backend
   #:.channel
   #:.clap-host-audio-ports
   #:.clap-host-gui
   #:.clap-process
   #:.events
   #:.event-input-bus-count
   #:.event-output-bus-count
   #:.ext-audio-ports
   #:.ext-note-ports
   #:.frames-per-buffer
   #:.inputs
   #:.input-events
   #:.key
   #:.hwnd
   #:.name
   #:.nchannels
   #:.notes
   #:.outputs
   #:.output-events
   #:.params-ordered
   #:.sample-offsets
   #:.steady-time
   #:.step-count
   #:.value
   #:.velocity
   #:.window
   #:app
   #:apply-from
   #:arrangement
   #:audio-bus
   #:audio-bus-buffers
   #:audio-device
   #:audio-device-window
   #:begin-edit
   #:buffer-at
   #:clip
   #:clip-audio
   #:clip-note
   #:closed
   #:cmd-add
   #:cmd-redo
   #:cmd-run
   #:cmd-undo
   #:color-window
   #:commander
   #:connection
   #:editor-close
   #:editor-open
   #:end-edit
   #:grid-mixin
   #:lane
   #:master-track
   #:memcpy
   #:module
   #:module-builtin
   #:module-clap
   #:module-fader
   #:module-fader-track
   #:module-gain
   #:module-gain-track
   #:module-track-mixin
   #:module-vst3
   #:neko
   #:neko-id
   #:note
   #:offset-mixin
   #:on-resize
   #:param
   #:params-prepare
   #:perform-edit
   #:piano-roll
   #:plugin-info
   #:plugin-info-vst3
   #:plugin-info-clap
   #:plugin-selector
   #:prepare
   #:preset
   #:preset-vst3
   #:process
   #:process-data
   #:project
   #:rack
   #:rect
   #:rect-piano-roll
   #:render
   #:render-context
   #:report-window
   #:request-callback
   #:request-hide
   #:request-resize
   #:request-show
   #:resize-hints-changed
   #:restart-component
   #:run-with-backend
   #:sceen
   #:sceen-matrix
   #:scroll-mixin
   #:seq
   #:seq-audio
   #:seq-note
   #:show-mixin
   #:terminate
   #:time-ruler-mixin
   #:time-thing
   #:track
   #:transposer
   #:value-changed-by-host
   #:value-text
   #:view
   #:with-debugger
   #:with-thraed-pool
   #:zoom-mixin
   ))

(defpackage :utaticl.ffi)

(defpackage :src-ffi
  (:use :cl))

(defpackage :ig-backend
  (:use :cl))

(defpackage :read-vst3-c-api-h)

(defpackage :grovel
  (:use :cl))

(defpackage :vst3-walk
  (:use :cl))

(defpackage :vst3-ffi)

(defpackage :vst3-impl
  (:use :cl))

(defpackage :vst3-host-application
  (:use :cl))

(defpackage :vst3
  (:use :cl :cffi)
  (:export
   #:from-string128))

(defpackage :win32
  (:use :cl))

(defpackage vulkan-backend
  (:use :cl))

(defpackage :dd-ffi
  (:use :cl))

(defpackage :utaticl.glfw-opengl3
  (:use :cl))

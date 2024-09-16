(defpackage :utaticl
  (:use :cl)
  (:export #:main))

(defpackage :utaticl.core
  (:use :cl :anaphora)
  (:export
   #:*app*
   #:*dd-at*
   #:*dd-srcs*
   #:*default-lane-width*
   #:*done*
   #:drag-enter
   #:drop
   #:*hwnd*
   #:*hwnd-module-vst3-map*
   #:*invoke-debugger-p*
   #:*mouse-pos*
   #:*process-data*
   #:*project*
   #:*render-context*
   #:*thread-pool*
   #:*working-directory*
   #:+side-threshold+
   #:.backend
   #:.name
   #:.params-ordered
   #:.hwnd
   #:.value
   #:.window
   #:app
   #:arrangement
   #:audio-bus-buffers
   #:audio-device
   #:audio-device-window
   #:begin-edit
   #:clip
   #:clip-audio
   #:clip-note
   #:cmd-add
   #:cmd-redo
   #:cmd-run
   #:cmd-undo
   #:color-window
   #:commander
   #:connection
   #:editor-close
   #:end-edit
   #:grid-mixin
   #:lane
   #:master-track
   #:memcpy
   #:module
   #:module-builtin
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

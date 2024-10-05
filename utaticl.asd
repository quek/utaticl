(ql:quickload :cffi)
(setf cffi:*foreign-library-directories*
      '("c:/Users/ancient/quicklisp/local-projects/utaticl/dll/"))

(initialize-source-registry
 '(:source-registry
   (:tree (:here "lib"))
   :inherit-configuration))

(defsystem :utaticl
  :licence "GPL3"
  :depends-on ("cffi-libffi"
               "sb-concurrency"
               "cl-portaudio"
               "vst3-c-api-autowrap"
               "clap"
               "sdl2"
               "vk"
               "cimgui-autowrap"
               "ftw"
               "anaphora"
               "log4cl"
               "uuid"
               "qbase64"
               "cl-wav"
               "cl-vorbis"
               "cl-glfw3"
               "cl-opengl")
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "prelude")
   (:file "protocol")
   (:file "utils")
   (:file "debugger")
   (:file "thread-pool")
   (:file "ring-buffer")
   (:file "grid")
   (:file "macro-serialize")
   (:file "config")
   (:file "classes")
   (:file "serialize")
   (:file "defserialize")
   (:file "process")
   (:file "render")
   (:file "library")
   (:file "audio-device")
   (:file "read-h")
   (:file "src-ffi")
   (:file "win32")
   (:file "ig")
   (:file "ig-backend")
   (:file "plugin-info")
   (:file "plugin-info-clap")
   (:file "plugin-info-vst3")
   (:file "vst3-macro")
   (:file "vst3-walk")
   (:file "vst3-ffi")
   (:file "vst3")
   (:file "module")
   (:file "vst3-impl")
   (:file "audio-bus")
   (:file "note-buffer")
   (:file "process-data")
   (:file "process-data-vst3")
   (:file "module-plugin")
   (:file "clap")
   (:file "module-clap")
   (:file "module-vst3")
   (:file "module-builtin")
   (:file "module-fader")
   (:file "module-gain")
   (:file "module-track-mixin")
   (:file "module-fader-track")
   (:file "preset-vst3")
   (:file "connection")
   (:file "param")
   (:file "param-clap")
   (:file "param-vst3")
   (:file "midi")
   (:file "commands")
   (:file "theme")
   (:file "ui-utils")
   (:file "neko")
   (:file "project")
   (:file "rack")
   (:file "track")
   (:file "master-track")
   (:file "lane")
   (:file "time-thing")
   (:file "clip")
   (:file "clip-audio")
   (:file "clip-note")
   (:file "seq")
   (:file "seq-audio")
   (:file "seq-note")
   (:file "note")
   (:file "view")
   (:file "drag-drop-ffi")
   (:file "drag-drop")
   (:file "report")
   (:file "transposer")
   (:file "arrangement")
   (:file "sceen")
   (:file "sceen-matrix")
   (:file "audio-bus-buffers")
   (:file "piano-roll")
   (:file "show-mixin")
   (:file "time-ruler-mixin")
   (:file "zoom-mixin")
   (:file "scroll-mixin")
   (:file "offset-mixin")
   (:file "grid-mixin")
   (:file "audio-device-window")
   (:file "color-window")
   (:file "commander")
   (:file "plugin-selector")
   (:file "app")
   (:file "vulkan-backend")
   (:file "glfw-opengl3")
   (:file "main")))

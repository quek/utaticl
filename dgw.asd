(ql:quickload :cffi)
(setf cffi:*foreign-library-directories*
      '("c:/Users/ancient/quicklisp/local-projects/dgw/dll/"))

(asdf:initialize-source-registry
 '(:source-registry
   (:tree (:here "lib"))
   :inherit-configuration))

(asdf:defsystem :dgw
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
               "qbase64")
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "prelude")
   (:file "protocol")
   (:file "utils")
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
   (:file "win32")
   (:file "ig")
   (:file "ig-backend")
   (:file "plugin-info")
   (:file "vst3-macro")
   (:file "vst3-walk")
   (:file "vst3-ffi")
   (:file "vst3")
   (:file "module")
   (:file "vst3-impl")
   (:file "module-vst3")
   (:file "module-clap")
   (:file "module-builtin")
   (:file "module-fader")
   (:file "module-gain")
   (:file "preset-vst3")
   (:file "connection")
   (:file "param")
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
   (:file "clip-note")
   (:file "seq-note")
   (:file "note")
   (:file "process-data")
   (:file "view")
   (:file "report")
   (:file "transposer")
   (:file "arrangement")
   (:file "audio-bus-buffers")
   (:file "piano-roll")
   (:file "show-mixin")
   (:file "time-ruler-mixin")
   (:file "zoom-mixin")
   (:file "scroll-mixin")
   (:file "offset-mixin")
   (:file "grid-mixin")
   (:file "audio-device-window")
   (:file "commander")
   (:file "plugin-selector")
   (:file "app")
   (:file "vulkan-backend")
   (:file "main")))

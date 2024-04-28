(asdf:initialize-source-registry
 '(:source-registry
   (:tree (:here "lib"))
   :inherit-configuration))

(asdf:defsystem :dgw
  :licence "GPL3"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi"
               "cffi-libffi"
               "sb-concurrency"
               "cl-portaudio"
               "vst3-c-api"
               "clap"
               "sdl2" "cl-opengl"
               "cimgui-autowrap"
               "ftw"
               "anaphora"
               "log4cl"
               "random-uuid")
  ;; :depends-on ("cl-autowrap/libffi")
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "prelude")
   (:file "utils")
   (:file "serialize")
   (:file "classes")
   (:file "library")
   (:file "audio-engine")
   (:file "read-h")
   (:file "win32")
   (:file "ig")
   (:file "ig-backend")
   (:file "plugin-info")
   (:file "vst3-macro")
   (:file "vst3-walk")
   (:file "vst3-ffi")
   ;;(:file "make-vst3-grovel")
   ;;(:cffi-grovel-file "XXXvst3-grovel")
   ;; (:cffi-grovel-file "vst3-grovel")
   (:file "vst3")
   (:file "module")
   (:file "vst3-impl")
   (:file "vst3-module")
   (:file "clap-module")
   (:file "midi")
   (:file "commands")
   (:file "theme")
   (:file "render")
   (:file "ui-utils")
   (:file "ui")
   (:file "neko")
   (:file "project")
   (:file "transposer")
   (:file "arrangement")
   (:file "rack")
   (:file "track")
   (:file "piano-roll")
   (:file "show-mixin")
   (:file "commander")
   (:file "plugin-selector")
   (:file "app")
   (:file "main")
   ;; (:module autowrap-spec
   ;;  :pathname "spec"
   ;;  :components ((:static-file "cimgui.h")))
   ))

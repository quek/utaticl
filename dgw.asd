 (eval-when (:compile-toplevel :load-toplevel :execute)
   (require :sb-grovel))

(asdf:defsystem :dgw
  :licence "GPL3"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-libffi" "sdl2" "cl-opengl" "vst3-c-api")
  ;; :depends-on ("cl-autowrap/libffi")
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "library")
   ;; (:file "autowrap")
   (:file "read-h")
   (:cffi-grovel-file "ig-grovel")
   ;(:cffi-wrapper-file "wrapper")
   (:file "ig")
   (:file "ig-backend")
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
   (:file "ui")
   ;; (:module autowrap-spec
   ;;  :pathname "spec"
   ;;  :components ((:static-file "cimgui.h")))
   ))

 (eval-when (:compile-toplevel :load-toplevel :execute)
   (require :sb-grovel))

(asdf:defsystem :dgw
  :licence "GPL3"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-libffi" "sdl2" "cl-opengl")
  ;; :depends-on ("cl-autowrap/libffi")
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "library")
   ;; (:file "autowrap")
   (:file "ffi-type")
   (:cffi-grovel-file "ig-grovel")
   ;(:cffi-wrapper-file "wrapper")
   (:file "ig")
   (:file "ig-backend")
   (:file "vst3-macro")
   (:file "vst3-grovel-maker")
   (:cffi-grovel-file "vst3-grovel")
   (:file "vst3")
   (:file "module")
   (:file "module-vst3")
   (:file "ui")
   ;; (:module autowrap-spec
   ;;  :pathname "spec"
   ;;  :components ((:static-file "cimgui.h")))
   ))

(in-package :dgw)

(cffi:define-foreign-library libcimgui
  (:windows "cimgui_sdl.dll"))

(cffi:use-foreign-library libcimgui)

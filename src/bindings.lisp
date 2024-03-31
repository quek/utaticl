(in-package :ig)

(define CIMGUI_DEFINE_ENUMS_AND_STRUCTS)

(include "c:/Users/ancient/quicklisp/local-projects/dgw/lib/cimgui/cimgui.h")

(cstruct ig::vec2 "ImVec2"
         (x "x" :type :float)
         (y "y" :type :float))

;; (cstruct-and-class-item ig::vec2 "ImVec2"
;;          (x "x" :type :float)
;;          (y "y" :type :float))

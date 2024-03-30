(in-package :dgw)

;; (setf autowrap:*c2ffi-program* "/home/ancient/c2ffi/build/bin/c2ffi")

(cffi:define-foreign-library libcimgui
  (:unix "/mnt/c/Users/ancient/quicklisp/local-projects/dgw/dll/cimgui.so")
  (:windows "cimgui.dll"))

(cffi:use-foreign-library libcimgui)

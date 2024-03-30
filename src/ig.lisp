(cl:in-package :ig)

(cffi:load-foreign-library "cimgui.dll")

(cffi:defcfun ("igCreateContext" create-context) :pointer
  (x :pointer))

(cffi:defcfun ("igNewFrame" new-frame) :void)

(cffi:defcfun ("igBegin" begin) :bool
  (name :string)
  (openp (:pointer :bool))
  (flags :int))

(cffi:defcfun ("igEnd" end) :void)

(cffi:defcfun ("igButton" button) :bool
  (label :string)
  (size (:struct ig::vec2)))

(cffi:defcfun ("igText" text) :void
  (fmt :string)
  &rest)

(cffi:defcfun ("igGetIO" get-io) :pointer)

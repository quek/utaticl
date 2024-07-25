(defpackage :dgw
  (:use :cl :anaphora :plus-c)
  (:export #:main))

(defpackage :dgw.ffi)

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
  (:use :cl :cffi))

(defpackage :win32
  (:use :cl))

(defpackage vulkan-backend
  (:use :cl))

(defpackage :dd-ffi
  (:use :cl))

(defpackage :dgw
  (:use :cl)
  (:export #:main))

(defpackage :dgw.ffi)

(defpackage :ig
  (:use :cl :cffi))

(defpackage :ig-backend
  (:use :cl))

(defpackage :read-vst3-c-api-h)

(defpackage :grovel
  (:use :cl)
  (:export #:char16))

(defpackage :vst3-walk
  (:use :cl))

(defpackage :vst3-ffi)

(defpackage :vst3
  (:use :cl :cffi)
  (:import-from :grovel #:char16))

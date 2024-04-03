(defpackage :dgw
  (:use :cl)
  (:export #:main #:char16))

(defpackage :dgw.ffi)

(defpackage :ig
  (:use :cl :cffi))

(defpackage :ig-backend
  (:use :cl))

(defpackage :vst3
  (:use :cl :cffi)
  (:import-from :dgw #:char16))

(defpackage :vst3-grovel
  (:use :cl))

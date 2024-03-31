(defpackage :dgw
  (:use :cl)
  (:export #:main))

(defpackage :dgw.ffi)

(defpackage :ig
  (:use :cl :cffi))

(defpackage :ig-backend
  (:use :cl))

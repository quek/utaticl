;; (cl:in-package :ig)
;; 
;; (defmethod translate-from-foreign (pointer (type vec2-tclass))
;;   (with-foreign-slots ((x y) pointer (:struct vec2))
;;     (list x y)))
;; 
;; (defmethod translate-into-foreign-memory (object (type vec2-tclass) pointer)
;;   (with-foreign-slots ((x y) pointer (:struct vec2))
;;     (setf x (car object)
;;           y (cadr object))))
;; 
;; ;; 定義しなくてもいい？
;; ;; (defmethod cffi:translate-to-foreign (object (type vec2-tclass))
;; ;;   (let ((p (foreign-alloc '(:struct vec2))))
;; ;;     (translate-into-foreign-memory object type p)
;; ;;     (values p t)))
;; 
;; ;; 定義しなくてもいい？
;; ;; (defmethod cffi:free-translated-object (pointer (type vec2-tclass) freep)
;; ;;   (when freep
;; ;;     (foreign-free pointer)))
;; 
;; 
;; (cffi:defcfun ("igCreateContext" create-context) :pointer
;;   (x :pointer))
;; 
;; (cffi:defcfun ("igSetCurrentContext" set-current-context) :void
;;   (x :pointer))
;; 
;; (cffi:defcfun ("igDestroyContext" destroy-context) :void
;;   (ctx :pointer))
;; 
;; (cffi:defcfun ("igNewFrame" new-frame) :void)
;; 
;; (cffi:defcfun ("igRender" render) :void)
;; 
;; (cffi:defcfun ("igGetDrawData" get-draw-data) :pointer)
;; 
;; (cffi:defcfun ("igBegin" begin) :bool
;;   (name :string)
;;   (openp (:pointer :bool))
;;   (flags :int))
;; 
;; (cffi:defcfun ("igEnd" end) :void)
;; 
;; (cffi:defcfun ("igButton" button) :bool
;;   (label :string)
;;   (size (:struct ig::vec2)))
;; 
;; (cffi:defcfun ("igText" text) :void
;;   (fmt :string)
;;   &rest)
;; 
;; (cffi:defcfun ("igGetIO" get-io) :pointer)

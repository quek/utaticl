(in-package :vst3-host-application)

(defclass unknown ()
  ((ptr :initarg :ptr :reader .ptr)
   (ref-count :initform 1 :accessor .ref-count)))

(defmethod add-ref ((self unknown))
  (incf (.ref-count self)))

(defmethod release ((self unknown))
  (decf (.ref-count self)))

(defun uid-equal (uid uid-ptr)
  (loop for i below 16
        always (= (aref uid i)
                  (cffi:mem-ref uid-ptr :uchar i))))

(defmacro %query-interface (self uid-lisp uid-c obj next)
  `(if (uid-equal ,uid-lisp ,uid-c)
       (progn
         (setf (cffi:mem-ref ,obj :pointer)
               (autowrap:ptr (.ptr ,self)))
         vst3-c-api:+steinberg-k-result-ok+)
       ,next))

(defmethod query-interface ((self unknown) iid obj)
  (%query-interface self vst3-ffi::+steinberg-funknown-iid+ iid obj
                    vst3-c-api:+steinberg-k-no-interface+))

(defclass host-application (unknown)
  ())

(defmethod query-interface ((self host-application) iid obj)
  (%query-interface self vst3-ffi::+steinberg-vst-ihost-application-iid+ iid obj
                            (call-next-method)))

(defvar *host-application*
  (make-instance 'host-application
                 :ptr (autowrap:alloc 'vst3-c-api:steinberg-vst-i-host-application)))

(autowrap:defcallback add-ref vst3-c-api:steinberg-uint32
    ((this-interface :pointer))
  (declare (ignore this-interface))
  (add-ref *host-application*))

(autowrap:defcallback release vst3-c-api:steinberg-uint32
    ((this-interface :pointer))
  (declare (ignore this-interface))
  (release *host-application*))

(autowrap:defcallback query-interface vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (iid :pointer)
     (obj :pointer))
  (declare (ignore this-interface))
  (query-interface *host-application* iid obj))

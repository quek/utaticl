(in-package :vst3)

(defconstant +kno-interface+ #x80004002)
(defconstant +kresult-ok+ #x00000000)
(defconstant +kresult-true+ #x00000000)
(defconstant +kresult-false+ #x00000001)
(defconstant +kinvalid-argument+ #x80070057)
(defconstant +knot-implemented+ #x80004001)
(defconstant +kinternal-error+ #x80004005)
(defconstant +knot-initialized+ #x8000ffff)
(defconstant +kout-of-memory+ #x8007000E)


(defun make-tuid (&rest args)
  "32ビット整数のリストから Steinberg_TUID を生成します。なんでこんな仕様なんだ？"
  (flet ((bytes1 (int32)
           (let ((bytes (make-array 4 :element-type 'unsigned-byte :initial-element 0)))
             (setf (aref bytes 0) (ldb (byte 8 0) int32))
             (setf (aref bytes 1) (ldb (byte 8 8) int32))
             (setf (aref bytes 2) (ldb (byte 8 16) int32))
             (setf (aref bytes 3) (ldb (byte 8 24) int32))
             bytes))
         (bytes2 (int32)
           (let ((bytes (make-array 4 :element-type 'unsigned-byte :initial-element 0)))
             (setf (aref bytes 0) (ldb (byte 8 16) int32))
             (setf (aref bytes 1) (ldb (byte 8 24) int32))
             (setf (aref bytes 2) (ldb (byte 8 0) int32))
             (setf (aref bytes 3) (ldb (byte 8 8) int32))
             bytes))(bytes34 (int32)
           (let ((bytes (make-array 4 :element-type 'unsigned-byte :initial-element 0)))
             (setf (aref bytes 0) (ldb (byte 8 24) int32))
             (setf (aref bytes 1) (ldb (byte 8 16) int32))
             (setf (aref bytes 2) (ldb (byte 8 8) int32))
             (setf (aref bytes 3) (ldb (byte 8 0) int32))
             bytes)))
    (let ((tuid (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
      (loop for i from 0 below (length args)
            do (let ((bytes (funcall (case i
                                       (0 #'bytes1)
                                       (1 #'bytes2)
                                       (t #'bytes34))
                                     (nth i args))))
                 (loop for j from 0 below 4
                       do (setf (aref tuid (+ (* i 4) j)) (aref bytes j)))))
      tuid)))

(defvar *iid-class-map* (make-hash-table))

(defvar *funknown-iid* (make-tuid #x00000000 #x00000000 #xC0000000 #x00000046))


(defun get-plugin-factory (vst3-path)
  ;; TODO どこかで library を close-foreign-library する
  (let ((library (cffi:load-foreign-library vst3-path)))
    (cffi:foreign-funcall-pointer
     (cffi:foreign-symbol-pointer "InitDll" :library library) ())
    (make-instance
     'plugin-factory
     :ptr (cffi:foreign-funcall-pointer
           (cffi:foreign-symbol-pointer "GetPluginFactory" :library library) ()
           :pointer))))


(defclass unknown ()
  ((ptr :initarg :ptr :accessor .ptr)
   (instance :accessor .instance)
   (vtbl :accessor .vtbl)))
(setf (gethash *funknown-iid* *iid-class-map*) 'unknown)

(defmethod initialize-instance :after ((self unknown) &key ptr)
  (setf (.instance self)
        (funcall (if (eq (type-of self) 'unknown)
                     #'make-funknown-from-pointer
                     (symbol-function (intern (format nil "MAKE-I~a-FROM-POINTER" (type-of self))
                                              :vst3)))
                 ptr))
  (setf (.vtbl self)
        (funcall (if (eq (type-of self) 'unknown)
                     #'make-funknown-vtbl-from-pointer
                     (symbol-function (intern (format nil "MAKE-I~a-VTBL-FROM-POINTER" (type-of self))
                                              :vst3)))
                 (funcall (if (eq (type-of self) 'unknown)
                              #'funknown-vtbl
                              (symbol-function (intern (format nil "I~a-VTBL" (type-of self))
                                                       :vst3)))
                          (.instance self))))
  (let ((release (funcall (if (eq (type-of self) 'unknown)
                              #'funknown-vtbl-release
                              (symbol-function (intern (format nil "I~a-VTBL-RELEASE" (type-of self))
                                                       :vst3)))
                          (.vtbl self))))
   (sb-ext:finalize self
                    (lambda ()
                      (print 'finalize)
                      (print
                       (cffi::foreign-funcall-pointer
                        release ()
                        :pointer ptr
                        :uint32))))))

(defmethod query-interface ((self unknown) iid)
  (cffi:with-foreign-objects ((ptr '(:pointer :void)))
    (if (= +kresult-ok+
           (call self query-interface
                 :pointer (sb-sys:vector-sap iid)
                 :pointer ptr
                 vst3::tresult))
        (make-instance (gethash iid *iid-class-map*)
                       :ptr (cffi:mem-ref ptr :pointer))
        nil)))

(defmethod release ((self unknown))
  (call self release :uint32))

(defvar *iplugin-factory-iid* (make-tuid #x7A4D811C #x52114A1F #xAED9D2EE #x0B43BF9F))

(defclass plugin-factory (unknown)
  ())
(setf (gethash *iplugin-factory-iid* *iid-class-map*) 'plugin-factory)

(defmethod count-classes ((self plugin-factory))
  (call self count-classes :int32))

(defvar *iplugin-factory2-iid* (make-tuid #x0007B650 #xF24B4C0B #xA464EDB9 #xF00B2ABB))

(defclass plugin-factory2 (plugin-factory) ())
(setf (gethash *iplugin-factory2-iid* *iid-class-map*) 'plugin-factory2)

(defvar *iplugin-factory3-iid* (make-tuid #x4555A2AB #xC1234E57 #x9B122910 #x36878931))

(defclass plugin-factory3 (plugin-factory2) ())
(setf (gethash *iplugin-factory3-iid* *iid-class-map*) 'plugin-factory3)

(defmethod get-class-info-unicode ((self plugin-factory3) index)
  (cffi:with-foreign-objects ((class-info '(:struct pclass-info-w)))
    (if (= +kresult-ok+
           (call self get-class-info-unicode
                 :int32 index
                 :pointer class-info
                 tresult))
        (make-pclass-info-w-from-pointer class-info)
        nil)))

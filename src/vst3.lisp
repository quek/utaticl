(in-package :vst3)

(cffi:defctype tresult :int32)

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
             bytes))
         (bytes34 (int32)
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
    (make-instance 'i-plugin-factory
                   :ptr (cffi:foreign-funcall-pointer
                         (cffi:foreign-symbol-pointer "GetPluginFactory" :library library) ()
                         :pointer))))

(defclass f-unknown ()
  ((ptr :initarg :ptr :accessor .ptr)
   (instance :accessor .instance)))
(setf (gethash *funknown-iid* *iid-class-map*) 'f-unknown)

(defmethod initialize-instance :after ((self f-unknown) &key ptr)
  (setf (.instance self)
        (funcall (symbol-function (intern (format nil "MAKE-STEINBERG-~a"
                                                  (type-of self))
                                          :vst3-c-api))
                 :ptr ptr))
  (let ((release (funcall (symbol-function
                           (intern (format nil
                                           "STEINBERG-~a.LP-VTBL*.RELEASE"
                                           (type-of self))
                                   :vst3-c-api))
                          (.instance self)))
        (type (type-of self)))
    (sb-ext:finalize self
                     (lambda ()
                       (print (list 'finalize type ptr))
                       (print
                        (cffi::foreign-funcall-pointer
                         release ()
                         :pointer ptr
                         :uint32))))))


(defmethod query-interface ((self f-unknown) iid)
  (cffi:with-foreign-objects ((ptr '(:pointer :void)))
    (if (= +kresult-ok+
           (call self query-interface
                 :pointer (sb-sys:vector-sap iid)
                 :pointer ptr
                 tresult))
        (make-instance (gethash iid *iid-class-map*)
                       :ptr (cffi:mem-ref ptr :pointer))
        nil)))

(defmethod release ((self f-unknown))
  (call self release :uint32))

(defvar *iplugin-factory-iid* (make-tuid #x7A4D811C #x52114A1F #xAED9D2EE #x0B43BF9F))

(defclass i-plugin-factory (f-unknown)
  ())
(setf (gethash *iplugin-factory-iid* *iid-class-map*) 'i-plugin-factory)

(defmethod count-classes ((self i-plugin-factory))
  (call self count-classes :int32))

(defmethod create-instance ((self i-plugin-factory) cid iid)
  (cffi:with-foreign-objects ((ptr :pointer))
    (if (= +kresult-ok+
           (call self create-instance
                 :pointer cid
                 :pointer (sb-sys:vector-sap iid)
                 :pointer ptr
                 tresult))
        (make-instance (gethash iid *iid-class-map*)
                       :ptr (cffi:mem-ref ptr :pointer))
        nil)))

(defvar *iplugin-factory2-iid* (make-tuid #x0007B650 #xF24B4C0B #xA464EDB9 #xF00B2ABB))

(defclass i-plugin-factory2 (i-plugin-factory) ())
(setf (gethash *iplugin-factory2-iid* *iid-class-map*) 'i-plugin-factory2)

(defvar *iplugin-factory3-iid* (make-tuid #x4555A2AB #xC1234E57 #x9B122910 #x36878931))

(defclass i-plugin-factory3 (i-plugin-factory2) ())
(setf (gethash *iplugin-factory3-iid* *iid-class-map*) 'i-plugin-factory3)

(defmethod create-component ((self i-plugin-factory3))
  (autowrap:with-many-alloc ((%class-info '(:struct (vst3-c-api::steinberg-p-class-info-w))))
    (loop for index below (count-classes self)
          for class-info = (if (= +kresult-ok+
                                  (call self get-class-info-unicode
                                        :int32 index
                                        :pointer (autowrap:ptr %class-info)
                                        tresult))
                               (make-instance 'class-info-w :pclass-info
                                              (vst3-c-api::make-steinberg-p-class-info-w :ptr (autowrap:ptr %class-info)))
                               nil)
            thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                         (vst3::create-instance self (vst3::.cid class-info) vst3::*icomponent-iid*)))))



(defmethod get-class-info-unicode ((self i-plugin-factory3) index)
  (cffi:with-foreign-objects ((class-info :pointer))
    (if (= +kresult-ok+
           (call self get-class-info-unicode
                 :int32 index
                 :pointer class-info
                 tresult))
        (make-instance 'class-info-w :pclass-info
                         (vst3-c-api::make-steinberg-p-class-info-w :ptr class-info))
        nil)))

(defclass class-info ()
  ((pclass-info :initarg :pclass-info :reader .pclass-info)))

(defclass class-info2 (class-info) ())

(defclass class-info-w (class-info2) ())

(defmethod .category ((self class-info-w))
  (cffi:foreign-string-to-lisp
   (vst3-c-api::steinberg-p-class-info-w.category[]& (.pclass-info self))
   :max-chars 32))

(defmethod .cid ((self class-info-w))
  (vst3-c-api::steinberg-p-class-info-w.cid& (.pclass-info self)))

(defvar *icomponent-iid* (make-tuid #xE831FF31 #xF2D54301 #x928EBBEE #x25697802))
(defclass vst-i-component (f-unknown)
  ())
(setf (gethash *icomponent-iid* *iid-class-map*) 'vst-i-component)

(defvar *iparameter-changes-iid* (make-tuid #xA4779663 #x0BB64A56 #xB44384A8 #x466FEB9D))

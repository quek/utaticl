(in-package :vst3)

sb:+k-result-ok+
;;⇒ 0
sb:+k-no-interface+
;;⇒ -2147467262
sb:+k-result-false+
;;⇒ 1
sb:+k-invalid-argument+
;;⇒ -2147024809
sb:+k-not-implemented+
;;⇒ -2147467263
sb:+k-internal-error+
;;⇒ -2147467259
sb:+k-not-initialized+
;;⇒ -2147418113
sb:+k-out-of-memory+
;;⇒ -2147024882


(define-condition vst3-error (error)
  ((code :initarg :code :reader .code))
  (:report (lambda (condition stream)
             (format stream "VST3 API Error! code: ~d ~a."
                     (.code condition)
                     (type-of condition)))))

(define-condition no-interface-error (vst3-error) ())
(define-condition invalid-argument-error (vst3-error) ())
(define-condition not-implemented-error (vst3-error) ())
(define-condition internal-error (vst3-error) ())
(define-condition not-initialized-error (vst3-error) ())
(define-condition out-of-memory-error (vst3-error) ())
(define-condition false-error (vst3-error) ())
(define-condition unknown-error (vst3-error) ())

(defmacro ensure-ok (form)
  (let ((result (gensym)))
    `(let ((,result ,form))
       (case ,result
         (#.sb:+k-result-ok+ ,result)
         (#.sb:+k-no-interface+ (error (make-condition 'no-interface-error :code ,result)))
         (#.sb:+k-invalid-argument+ (error (make-condition 'invalid-argument-error :code ,result)))
         (#.sb:+k-not-implemented+ (error (make-condition 'not-implemented-error :code ,result)))
         (#.sb:+k-internal-error+ (error (make-condition 'internal-error :code ,result)))
         (#.sb:+k-not-initialized+ (error (make-condition 'not-initialized-error :code ,result)))
         (#.sb:+k-out-of-memory+ (error (make-condition 'out-of-memory-error :code ,result)))
         (#.sb:+k-result-false+ (error (make-condition 'false-error :code ,result)))
         (t (error (make-condition 'unknown-error :code ,result)))))))

(defun get-plugin-factory (vst3-path)
  ;; cffi:foreign-symbol-pointer の library 引数が使われないので LoadLibraryA を使う
  (let* ((lib (cffi:foreign-funcall "LoadLibraryA" :string vst3-path :pointer))
         (init-dll (cffi:foreign-funcall "GetProcAddress" :pointer lib :string "InitDll" :pointer)))
    (unless (cffi:foreign-funcall-pointer init-dll () :bool)
      (error "InitDll Failed! ~a" vst3-path))
    (let ((get-plugin-factory (cffi:foreign-funcall "GetProcAddress" :pointer lib :string "GetPluginFactory" :pointer)))
      (when (cffi:null-pointer-p get-plugin-factory)
        (error "No GetPluginFactory! ~a" vst3-path))
      (let ((plugin-factory (cffi:foreign-funcall-pointer get-plugin-factory () :pointer)))
        (when (cffi:null-pointer-p plugin-factory)
          (error "GetPluginFactory Failed! ~a" vst3-path))
        (values (make-instance 'vst3-ffi::iplugin-factory :ptr plugin-factory)
                lib)))))

(defun unload-library (library)
  (let ((exit-dll (cffi:foreign-funcall "GetProcAddress" :pointer library :string "ExitDll" :pointer)))
    (cffi:foreign-funcall-pointer exit-dll () :bool))
  (cffi:foreign-funcall "FreeLibrary" :pointer library :int))

(defmethod query-interface (self iid)
  (cffi:with-foreign-object (obj :pointer)
    (ensure-ok (vst3-ffi::query-interface self iid obj))
    (make-instance (gethash iid vst3-walk::*iid-class-map*)
                   :ptr (cffi:mem-ref obj :pointer))))

(defmethod create-component ((self vst3-ffi::iplugin-factory))
  (destructuring-bind (instance cid) (%create-component self)
    (values instance cid)))

(defmethod create-component-by-id ((self vst3-ffi::iplugin-factory) id)
  (autowrap:with-many-alloc ((%class-info '(:struct (sb:p-class-info))))
    (loop for index below (vst3-ffi::count-classes self)
          for class-info = (progn (ensure-ok
                                   (vst3-ffi::get-class-info self index (autowrap:ptr %class-info)))
                                  (sb::make-p-class-info :ptr (autowrap:ptr %class-info)))
            thereis (and (loop for i below 16
                               always (= (aref id i)
                                         (cffi:mem-aref (vst3::.cid class-info) :unsigned-char  i)))
                         (create-instance self (vst3::.cid class-info) vst3-ffi::+vst-icomponent-iid+)))))

(flet ((%create-instance (self class-info)
         (if (equal (vst3::.category class-info) "Audio Module Class")
             (let ((instance (create-instance self (vst3::.cid class-info) vst3-ffi::+vst-icomponent-iid+)))
               (if instance
                   (list instance
                         (let ((cid (make-array 16 :element-type '(unsigned-byte 8))))
                           (loop for i below 16
                                 do (setf (aref cid i)
                                          (cffi:mem-aref (vst3::.cid class-info) :unsigned-char i)))
                           cid)))))))

  (defmethod %create-component ((self vst3-ffi::iplugin-factory))
    (autowrap:with-many-alloc ((%class-info '(:struct (sb:p-class-info))))
      (loop for index below (vst3-ffi::count-classes self)
            for class-info = (progn (ensure-ok
                                     (vst3-ffi::get-class-info self index (autowrap:ptr %class-info)))
                                    (sb::make-p-class-info :ptr (autowrap:ptr %class-info)))
              thereis (%create-instance self class-info))))

  (defmethod %create-component ((self vst3-ffi::iplugin-factory2))
    (autowrap:with-many-alloc ((%class-info '(:struct (sb:p-class-info2))))
      (loop for index below (vst3-ffi::count-classes self)
            for class-info = (progn (ensure-ok
                                     (vst3-ffi::get-class-info2 self index (autowrap:ptr %class-info)))
                                    (sb::make-p-class-info2 :ptr (autowrap:ptr %class-info)))
              thereis (%create-instance self class-info))))

  (defmethod %create-component ((self vst3-ffi::iplugin-factory3))
    (autowrap:with-many-alloc ((%class-info '(:struct (sb:p-class-info-w))))
      (loop for index below (vst3-ffi::count-classes self)
            for class-info = (progn (ensure-ok
                                     (vst3-ffi::get-class-info-unicode self index (autowrap:ptr %class-info)))
                                    (sb::make-p-class-info-w :ptr (autowrap:ptr %class-info)))
              thereis (%create-instance self class-info)))))

(defmethod create-instance ((self vst3-ffi::iplugin-factory) (cid simple-array) iid)
  (sb-sys:with-pinned-objects (cid)
    (create-instance self (sb-sys:vector-sap cid) iid)))

(defmethod create-instance ((self vst3-ffi::iplugin-factory) cid iid)
  (cffi:with-foreign-objects ((ptr :pointer))
    (ensure-ok
     (sb-sys:with-pinned-objects (iid)
       (vst3-ffi::create-instance self cid (sb-sys:vector-sap iid) ptr)))
    (make-instance (gethash iid vst3-walk::*iid-class-map*)
                   :ptr (cffi:mem-ref ptr :pointer))))

(defmethod .category ((self sb:p-class-info))
  (cffi:foreign-string-to-lisp
   (sb:p-class-info.category[]& self)
   :max-chars 32))

(defmethod .category ((self sb:p-class-info2))
  (cffi:foreign-string-to-lisp
   (sb:p-class-info2.category[]& self)
   :max-chars 32))

(defmethod .category ((self sb:p-class-info-w))
  (cffi:foreign-string-to-lisp
   (sb:p-class-info-w.category[]& self)
   :max-chars 32))

(defmethod .cid ((self sb:p-class-info))
  (sb:p-class-info.cid& self))

(defmethod .cid ((self sb:p-class-info2))
  (sb:p-class-info2.cid& self))

(defmethod .cid ((self sb:p-class-info-w))
  (sb:p-class-info-w.cid& self))

(defmethod get-controller-class-id ((self vst3-ffi::vst-icomponent))
  (let ((id (make-array 16 :element-type '(unsigned-byte 8))))
    (ensure-ok (vst3-ffi::get-controller-class-id self id))
    id))

(defun plugin-scan-vst3 (&optional (dir "c:\\Program Files\\Common Files\\VST3"))
  (loop for %path in (directory (merge-pathnames "**/*.vst3" dir))
        for path = (namestring %path)
        for file-write-date = (file-write-date path)
        unless (uiop:directory-pathname-p path)
          nconc (multiple-value-bind (factory library) (vst3::get-plugin-factory path)
                  (unwind-protect
                       (autowrap:with-alloc (%class-info '(:struct (sb:p-class-info)))
                         (loop for index below (vst3-ffi::count-classes factory)
                               for class-info = (progn (ensure-ok
                                                        (vst3-ffi::get-class-info
                                                         factory index (autowrap:ptr %class-info)))
                                                       (sb::make-p-class-info
                                                        :ptr (autowrap:ptr %class-info)))
                               if (equal (vst3::.category class-info) "Audio Module Class")
                                 collect (make-instance
                                          'utaticl::plugin-info-vst3
                                          :id (let ((cid (make-array 16 :element-type '(unsigned-byte 8)))
                                                    (p (sb:p-class-info.cid& class-info)))
                                                (loop for i below 16 do
                                                  (setf (aref cid i)
                                                        (autowrap:c-aref p i :unsigned-char)))
                                                cid)
                                          :name (cffi:foreign-string-to-lisp
                                                 (sb:p-class-info.name[]& class-info))
                                          :path path
                                          :file-write-date file-write-date)))
                    (vst3-ffi::release factory)
                    (sb-ext:cancel-finalization factory)
                    (vst3::unload-library library)))))
#+nil
(sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
  (plugin-scan-vst3))

(defun from-string128 (string128)
  (let* ((buffer (make-array (* 128 2) :element-type '(unsigned-byte 8) :initial-element 0))
         (length (loop for i below 128
                       for c = (cffi:mem-aref string128 :uint16 i)
                       while (plusp c)
                       do (setf (aref buffer (* i 2)) (ldb (byte 8 0) c))
                          (setf (aref buffer (1+ (* i 2))) (ldb (byte 8 8) c))
                       finally (return (* i 2)))))
    (sb-ext:octets-to-string buffer
                             :external-format :utf-16le
                             :end length)))



(in-package :vst3)

vst3-c-api:+steinberg-k-result-ok+
;;⇒ 0
vst3-c-api:+steinberg-k-no-interface+
;;⇒ -2147467262
vst3-c-api:+steinberg-k-result-false+
;;⇒ 1
vst3-c-api:+steinberg-k-invalid-argument+
;;⇒ -2147024809
vst3-c-api:+steinberg-k-not-implemented+
;;⇒ -2147467263
vst3-c-api:+steinberg-k-internal-error+
;;⇒ -2147467259
vst3-c-api:+steinberg-k-not-initialized+
;;⇒ -2147418113
vst3-c-api:+steinberg-k-out-of-memory+
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

(defmacro ensure-ok (form)
  (let ((result (gensym)))
    `(let ((,result ,form))
       (case ,result
         (#.vst3-c-api::+steinberg-k-result-ok+ ,result)
         (#.vst3-c-api:+steinberg-k-no-interface+ (error (make-condition 'no-interface-error :code ,result)))
         (#.vst3-c-api:+steinberg-k-invalid-argument+ (error (make-condition 'invalid-argument-error :code ,result)))
         (#.vst3-c-api:+steinberg-k-not-implemented+ (error (make-condition 'not-implemented-error :code ,result)))
         (#.vst3-c-api:+steinberg-k-internal-error+ (error (make-condition 'internal-error :code ,result)))
         (#.vst3-c-api:+steinberg-k-not-initialized+ (error (make-condition 'not-initialized-error :code ,result)))
         (#.vst3-c-api:+steinberg-k-out-of-memory+ (error (make-condition 'out-of-memory-error :code ,result)))))))

(defun get-plugin-factory (vst3-path)
  ;; TODO どこかで library を close-foreign-library する
  (let ((library (cffi:load-foreign-library vst3-path)))
    (cffi:foreign-funcall-pointer
     (cffi:foreign-symbol-pointer "InitDll" :library library) ())
    (let ((plugin-factory (cffi:foreign-funcall-pointer
                           (cffi:foreign-symbol-pointer "GetPluginFactory" :library library) ()
                           :pointer)))
      (make-instance 'vst3-ffi::steinberg-iplugin-factory :ptr plugin-factory))))

(defmethod query-interface (self iid)
  (cffi:with-foreign-object (obj :pointer)
    (ensure-ok (vst3-ffi::query-interface self iid obj))
    (make-instance (gethash iid vst3-walk::*iid-class-map*)
                   :ptr (cffi:mem-ref obj :pointer))))

(defmethod create-component ((self vst3-ffi::steinberg-iplugin-factory))
  (let ((factory (or (query-interface self vst3-ffi::+steinberg-iplugin-factory3-iid+)
                     (query-interface self vst3-ffi::+steinberg-iplugin-factory2-iid+)
                     self)))
    (%create-component factory)))

(defmethod %create-component ((self vst3-ffi::steinberg-iplugin-factory))
  (autowrap:with-many-alloc ((%class-info '(:struct (vst3-c-api::steinberg-p-class-info))))
    (loop for index below (vst3-ffi::count-classes self)
          for class-info = (progn (ensure-ok
                                   (vst3-ffi::get-class-info self index (autowrap:ptr %class-info)))
                                  (vst3-c-api::make-steinberg-p-class-info :ptr (autowrap:ptr %class-info)))
            thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                         (create-instance self (vst3::.cid class-info) vst3-ffi::+steinberg-vst-icomponent-iid+)))))

(defmethod %create-component ((self vst3-ffi::steinberg-iplugin-factory2))
  (autowrap:with-many-alloc ((%class-info '(:struct (vst3-c-api::steinberg-p-class-info2))))
    (loop for index below (vst3-ffi::count-classes self)
          for class-info = (progn (ensure-ok
                                   (vst3-ffi::get-class-info2 self index (autowrap:ptr %class-info)))
                                  (vst3-c-api::make-steinberg-p-class-info2 :ptr (autowrap:ptr %class-info)))
            thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                         (create-instance self (vst3::.cid class-info) vst3-ffi::+steinberg-vst-icomponent-iid+)))))

(defmethod %create-component ((self vst3-ffi::steinberg-iplugin-factory3))
  (autowrap:with-many-alloc ((%class-info '(:struct (vst3-c-api::steinberg-p-class-info-w))))
    (loop for index below (vst3-ffi::count-classes self)
          for class-info = (progn (ensure-ok
                                   (vst3-ffi::get-class-info-unicode self index (autowrap:ptr %class-info)))
                                  (vst3-c-api::make-steinberg-p-class-info-w :ptr (autowrap:ptr %class-info)))
            thereis (and (equal (vst3::.category class-info) "Audio Module Class")
                         (create-instance self (vst3::.cid class-info) vst3-ffi::+steinberg-vst-icomponent-iid+)))))

(defmethod create-instance ((self vst3-ffi::steinberg-iplugin-factory) (cid simple-array) iid)
  (create-instance self (sb-sys:vector-sap cid) iid))

(defmethod create-instance ((self vst3-ffi::steinberg-iplugin-factory) cid iid)
  (cffi:with-foreign-objects ((ptr :pointer))
    (ensure-ok (vst3-ffi::create-instance self cid (sb-sys:vector-sap iid) ptr))
    (make-instance (gethash iid vst3-walk::*iid-class-map*)
                   :ptr (cffi:mem-ref ptr :pointer))))

(defmethod .category ((self vst3-c-api:steinberg-p-class-info))
  (cffi:foreign-string-to-lisp
   (vst3-c-api::steinberg-p-class-info.category[]& self)
   :max-chars 32))

(defmethod .category ((self vst3-c-api:steinberg-p-class-info2))
  (cffi:foreign-string-to-lisp
   (vst3-c-api::steinberg-p-class-info2.category[]& self)
   :max-chars 32))

(defmethod .category ((self vst3-c-api:steinberg-p-class-info-w))
  (cffi:foreign-string-to-lisp
   (vst3-c-api::steinberg-p-class-info-w.category[]& self)
   :max-chars 32))

(defmethod .cid ((self vst3-c-api:steinberg-p-class-info))
  (vst3-c-api::steinberg-p-class-info.cid& self))

(defmethod .cid ((self vst3-c-api:steinberg-p-class-info2))
  (vst3-c-api::steinberg-p-class-info2.cid& self))

(defmethod .cid ((self vst3-c-api:steinberg-p-class-info-w))
  (vst3-c-api::steinberg-p-class-info-w.cid& self))

(defmethod get-controller-class-id ((self vst3-ffi::steinberg-vst-icomponent))
  (let ((id (make-array 16 :element-type '(unsigned-byte 8))))
    (ensure-ok (vst3-ffi::get-controller-class-id self id))
    id))

(in-package :vst3)

(defmacro ensure-ok (form)
  (let ((result (gensym)))
    `(let ((,result ,form))
       (if (= ,result vst3-c-api::+steinberg-k-result-ok+)
           ,result
           (error "error ~d" ,result)))))

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

(in-package :vst3-impl)

(defvar *ptr-object-map* (make-hash-table :weakness :value))

(defclass unknown ()
  ((wrap :reader .wrap)
   (vtbl :reader .vtbl)
   (ref-count :initform 1 :accessor .ref-count)))

(defmethod ptr ((self unknown))
  (autowrap:ptr (.wrap self)))

(defmethod initialize-instance :before ((self unknown) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'vst3-c-api:steinberg-f-unknown-vtbl))
          (wrap (autowrap:alloc 'vst3-c-api:steinberg-f-unknown)))
      (setf (vst3-c-api:steinberg-f-unknown.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))

  (let ((vtbl (if (typep (.vtbl self) 'vst3-c-api::steinberg-f-unknown-vtbl)
                  (.vtbl self)
                  (vst3-c-api::make-steinberg-f-unknown-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (vst3-c-api:steinberg-f-unknown-vtbl.add-ref vtbl)
          (autowrap:callback 'add-ref))
    (setf (vst3-c-api:steinberg-f-unknown-vtbl.release vtbl)
          (autowrap:callback 'release))
    (setf (vst3-c-api:steinberg-f-unknown-vtbl.query-interface vtbl)
          (autowrap:callback 'query-interface))))

(defmethod initialize-instance :after ((self unknown) &key)
  (setf (gethash (cffi:pointer-address (autowrap:ptr (.wrap self)))
                 *ptr-object-map*)
        self)
  (let ((vtbl (.vtbl self))
        (wrap (.wrap self))
        (class (type-of self)))
    (sb-ext:finalize self
                     (lambda ()
                       (print (list 'finalize class vtbl wrap))
                       (when (autowrap:valid-p vtbl)
                         (autowrap:free vtbl)
                         (autowrap:invalidate vtbl))
                       (when (autowrap:valid-p wrap)
                         (autowrap:free wrap)
                         (autowrap:invalidate wrap))))))

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
               (autowrap:ptr (.wrap ,self)))
         vst3-c-api:+steinberg-k-result-ok+)
       ,next))

(defmethod query-interface ((self unknown) iid obj)
  (%query-interface self vst3-ffi::+steinberg-funknown-iid+ iid obj
                    vst3-c-api:+steinberg-k-no-interface+))

(defclass host-application (unknown)
  ((module :initarg :module :accessor .module)
   (component-handler :accessor .component-handler)))

(defmethod initialize-instance :before ((self host-application) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'vst3-c-api:steinberg-vst-i-host-application-vtbl))
          (wrap (autowrap:alloc 'vst3-c-api:steinberg-vst-i-host-application)))
      (setf (vst3-c-api:steinberg-vst-i-host-application.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))
  (let ((vtbl (if (typep (.vtbl self) 'vst3-c-api::steinberg-vst-i-host-application-vtbl)
                  (.vtbl self)
                  (vst3-c-api::make-steinberg-vst-i-host-application-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (vst3-c-api::steinberg-vst-i-host-application-vtbl.get-name vtbl)
          (autowrap:callback 'get-name))
    (setf (vst3-c-api::steinberg-vst-i-host-application-vtbl.create-instance vtbl)
          (autowrap:callback 'create-instance))))

(defmethod initialize-instance :after ((self host-application) &key module)
  (setf (slot-value self 'component-handler)
        (make-instance 'component-handler :module module)))

(defmethod query-interface ((self host-application) iid obj)
  (%query-interface
   self vst3-ffi::+steinberg-vst-ihost-application-iid+ iid obj
   (if (uid-equal vst3-ffi::+steinberg-vst-icomponent-handler-iid+ iid)
       (progn
         (setf (cffi:mem-ref obj :pointer) (autowrap:ptr (.wrap (.component-handler self))))
         vst3-c-api:+steinberg-k-result-ok+)
       (call-next-method))))

(defmethod get-name ((self host-application) name)
  (let* ((host-name "DGW")
         (array (sb-ext:string-to-octets host-name :external-format :utf16le)))
    (loop for i below (length host-name)
          do (setf (cffi:mem-ref name :int16 i)
                   (cffi:mem-ref (sb-sys:vector-sap array) :int16 i)))
    vst3-c-api:+steinberg-k-result-ok+))

(defmethod create-instance ((self host-application) cid iid obj)
  ;; TODO
  (list self cid iid obj)
  vst3-c-api:+steinberg-k-no-interface+)

(autowrap:defcallback add-ref vst3-c-api:steinberg-uint32
    ((this-interface :pointer))
  (add-ref (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))

(autowrap:defcallback release vst3-c-api:steinberg-uint32
    ((this-interface :pointer))
  (let ((obj (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))
    (print (list 'release obj this-interface))
    (release obj)))

(autowrap:defcallback query-interface vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (iid :pointer)
     (obj :pointer))
  (query-interface (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                   iid obj))

(autowrap:defcallback get-name vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (name :pointer))
  (get-name (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
            name))

(autowrap:defcallback create-instance vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (cid :pointer)
     (iid :pointer)
     (obj :pointer))
  (create-instance (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                   cid iid obj))

(defclass component-handler (unknown)
  ((module :initarg :module :accessor .module)))

(defmethod initialize-instance :before ((self component-handler) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'vst3-c-api:steinberg-vst-i-component-handler-vtbl))
          (wrap (autowrap:alloc 'vst3-c-api:steinberg-vst-i-component-handler)))
      (setf (vst3-c-api:steinberg-vst-i-component-handler.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))
  (let ((vtbl (if (typep (.vtbl self) 'vst3-c-api::steinberg-vst-i-component-handler-vtbl)
                  (.vtbl self)
                  (vst3-c-api::make-steinberg-vst-i-component-handler-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (vst3-c-api::steinberg-vst-i-component-handler-vtbl.begin-edit vtbl)
          (autowrap:callback 'begin-edit))
    (setf (vst3-c-api::steinberg-vst-i-component-handler-vtbl.perform-edit vtbl)
          (autowrap:callback 'perform-edit))
    (setf (vst3-c-api::steinberg-vst-i-component-handler-vtbl.end-edit vtbl)
          (autowrap:callback 'end-edit))
    (setf (vst3-c-api::steinberg-vst-i-component-handler-vtbl.restart-component vtbl)
          (autowrap:callback 'restart-component))))

(defmethod begin-edit ((self component-handler) id)
  (dgw::begin-edit (.module self) id)
  vst3-c-api::+steinberg-k-result-ok+)

(autowrap:defcallback begin-edit vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (id :unsigned-int))
  (begin-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
              id))

(defmethod perform-edit ((self component-handler) id value-normalized)
  (dgw::perform-edit (.module self) id value-normalized)
  vst3-c-api::+steinberg-k-result-ok+)

(autowrap:defcallback perform-edit vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (id :unsigned-int)
     (value-normalized :double))
  (perform-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
               id value-normalized))

(defmethod end-edit ((self component-handler) id)
  (dgw::end-edit (.module self) id)
  vst3-c-api::+steinberg-k-result-ok+)

(autowrap:defcallback end-edit vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (id :unsigned-int))
  (end-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
              id))

(defmethod restart-component ((self component-handler) flags)
  (dgw::restart-component (.module self) flags)
  vst3-c-api::+steinberg-k-result-ok+)

(autowrap:defcallback restart-component vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (flags :int))
  (restart-component (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                     flags))

(defclass bstream (unknown)
  ((buffer :initform (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
           :reader .buffer)
   (cursor :initform 0 :accessor .cursor)))

(defmethod initialize-instance :before ((self bstream) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'vst3-c-api:steinberg-ib-stream-vtbl))
          (wrap (autowrap:alloc 'vst3-c-api:steinberg-ib-stream)))
      (setf (vst3-c-api:steinberg-ib-stream.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))

  (let ((vtbl (if (typep (.vtbl self) 'vst3-c-api::steinberg-ib-stream-vtbl)
                  (.vtbl self)
                  (vst3-c-api::make-steinberg-ib-stream-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (vst3-c-api:steinberg-ib-stream-vtbl.read vtbl)
          (autowrap:callback '.read))
    (setf (vst3-c-api:steinberg-ib-stream-vtbl.write vtbl)
          (autowrap:callback '.write))
    (setf (vst3-c-api:steinberg-ib-stream-vtbl.seek vtbl)
          (autowrap:callback 'seek))
    (setf (vst3-c-api:steinberg-ib-stream-vtbl.tell vtbl)
          (autowrap:callback 'tell))))

(defmethod .read ((self bstream) buffer num-bytes num-bytes-read)
  (let ((len (min (- (length (.buffer self)) (.cursor self))
                  num-bytes)))
    (loop for i below len
          do (setf (cffi:mem-ref buffer :uchar i)
                   (aref (.buffer self) (+ i (.cursor self)))))
    (unless (cffi:null-pointer-p num-bytes-read)
      (setf (cffi:mem-ref num-bytes-read :int32) len))
    (incf (.cursor self) len)
    vst3-c-api:+steinberg-k-result-ok+))

(autowrap:defcallback .read vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (buffer :pointer)
     (num-bytes :int)
     (num-bytes-read :pointer))
  (.read (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
         buffer num-bytes num-bytes-read))

(defmethod .write ((self bstream) buffer num-bytes num-bytes-written)
  (loop for i below num-bytes
        if (< (+ i (.cursor self)) (length (.buffer self)))
          do (setf (aref (.buffer self) (+ i (.cursor self)))
                   (cffi:mem-ref buffer :uchar i))
        else
          do (vector-push-extend (cffi:mem-ref buffer :uchar i)
                                 (.buffer self)))
  (incf (.cursor self) num-bytes)
  (unless (cffi:null-pointer-p num-bytes-written)
    (setf (cffi:mem-ref num-bytes-written :int32) num-bytes))
  vst3-c-api::+steinberg-k-result-ok+)

(autowrap:defcallback .write vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (buffer :pointer)
     (num-bytes :int)
     (num-bytes-written :pointer))
  (.write (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
          buffer num-bytes num-bytes-written))

(defmethod seek ((self bstream) pos mod result)
  (setf (.cursor self)
        (case mod
          (#.vst3-c-api:+steinberg-ib-stream-i-stream-seek-mode-k-ib-seek-set+
           (max pos (length (.buffer self))))
          (#.vst3-c-api:+steinberg-ib-stream-i-stream-seek-mode-k-ib-seek-cur+
           (max (+ pos (.cursor self)) (length (.buffer self))))
          (#.vst3-c-api:+steinberg-ib-stream-i-stream-seek-mode-k-ib-seek-end+
           (min 0 (- (length (.buffer self)) pos)))))
  (setf (cffi:mem-ref result :int64) (.cursor self))
  vst3-c-api::+steinberg-k-result-ok+)

(autowrap:defcallback seek vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (pos :long-long)
     (mod :int)
     (result :pointer))
  (seek (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
          pos mod result))

(defmethod tell ((self bstream) pos)
  (setf (cffi:mem-ref pos :int64) (.cursor self))
  vst3-c-api::+steinberg-k-result-ok+)

(autowrap:defcallback tell vst3-c-api::steinberg-tresult
    ((this-interface :pointer)
     (pos :pointer))
  (tell (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
        pos))

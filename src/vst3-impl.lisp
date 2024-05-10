(in-package :vst3-impl)

(defvar *ptr-object-map* (make-hash-table :weakness :value))

(defun print-uid-ptr (ptr)
  (format t "~%0x~2,'0X" (cffi:mem-ref ptr :uint8 3))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 2))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 1))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 0))
  (format t ", 0x~2,'0X" (cffi:mem-ref ptr :uint8 5))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 4))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 7))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 6))
  (format t ", 0x~2,'0X" (cffi:mem-ref ptr :uint8 8))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 9))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 10))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 11))
  (format t ", 0x~2,'0X" (cffi:mem-ref ptr :uint8 12))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 13))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 14))
  (format t "~2,'0X" (cffi:mem-ref ptr :uint8 15)))

(defclass unknown ()
  ((wrap :reader .wrap)
   (vtbl :reader .vtbl)
   (ref-count :initform 0 :accessor .ref-count)))

(defmethod ptr ((self unknown))
  (autowrap:ptr (.wrap self)))

(defmethod initialize-instance :before ((self unknown) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'sb:f-unknown-vtbl))
          (wrap (autowrap:alloc 'sb:f-unknown)))
      (setf (sb:f-unknown.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))

  (let ((vtbl (if (typep (.vtbl self) 'sb:f-unknown-vtbl)
                  (.vtbl self)
                  (sb::make-f-unknown-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (sb:f-unknown-vtbl.add-ref vtbl)
          (autowrap:callback 'add-ref))
    (setf (sb:f-unknown-vtbl.release vtbl)
          (autowrap:callback 'release))
    (setf (sb:f-unknown-vtbl.query-interface vtbl)
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

(autowrap:defcallback add-ref sb:uint32
    ((this-interface :pointer))
  (add-ref (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))

(defmethod release ((self unknown))
  (let ((ref-count (decf (.ref-count self))))
    (when (zerop ref-count)
        (print (list 'free-in-release self (.vtbl self) (.wrap self)))
        (autowrap:free (.vtbl self))
        (autowrap:invalidate (.vtbl self))
        (autowrap:free (.wrap self))
        (autowrap:invalidate (.wrap self))
        (sb-ext:cancel-finalization self))
    ref-count))

(autowrap:defcallback release sb:uint32
    ((this-interface :pointer))
  (let ((obj (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))
    (print (list 'release obj this-interface))
    (if obj
        (release obj)
        ;; finalizer で削除済
        0)))

(defun uid-equal (uid uid-ptr)
  (loop for i below 16
        always (= (aref uid i)
                  (cffi:mem-ref uid-ptr :uchar i))))

(defmacro %query-interface (self uid-lisp uid-c obj next)
  `(if (uid-equal ,uid-lisp ,uid-c)
       (progn
         (setf (cffi:mem-ref ,obj :pointer)
               (autowrap:ptr (.wrap ,self)))
         (add-ref self)
         sb:+k-result-ok+)
       ,next))

(defmethod query-interface ((self unknown) iid obj)
  (%query-interface self vst3-ffi::+funknown-iid+ iid obj
                    sb:+k-no-interface+))

(autowrap:defcallback query-interface sb:tresult
    ((this-interface :pointer)
     (iid :pointer)
     (obj :pointer))
  (print-uid-ptr iid)
  (query-interface (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                   iid obj))

(defclass host-application (unknown)
  ((module :initarg :module :accessor .module)
   (component-handler :accessor .component-handler)
   (plug-frame :accessor .plug-frame)))

(defmethod initialize-instance :before ((self host-application) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'sb:vst-i-host-application-vtbl))
          (wrap (autowrap:alloc 'sb:vst-i-host-application)))
      (setf (sb:vst-i-host-application.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))
  (let ((vtbl (if (typep (.vtbl self) 'sb:vst-i-host-application-vtbl)
                  (.vtbl self)
                  (sb::make-vst-i-host-application-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (sb:vst-i-host-application-vtbl.get-name vtbl)
          (autowrap:callback 'get-name))
    (setf (sb:vst-i-host-application-vtbl.create-instance vtbl)
          (autowrap:callback 'create-instance))))

(defmethod initialize-instance :after ((self host-application) &key module)
  (setf (slot-value self 'component-handler)
        (make-instance 'component-handler2 :module module))
  (setf (slot-value self 'plug-frame)
        (make-instance 'plug-frame :module module)))

(defmethod query-interface ((self host-application) iid obj)
  (%query-interface
   self vst3-ffi::+vst-ihost-application-iid+ iid obj
   (query-interface (.component-handler self) iid obj)))

(defmethod get-name ((self host-application) name)
  (let* ((host-name "DGW")
         (array (sb-ext:string-to-octets host-name :external-format :utf16le))
         (length (length array)))
    (loop for i below length
          do (setf (cffi:mem-ref name :char i)
                   (aref array i)))
    (setf (cffi:mem-ref name :char length) 0)
    (setf (cffi:mem-ref name :char (1+ length)) 0)
    sb:+k-result-ok+))

(autowrap:defcallback get-name sb:tresult
    ((this-interface :pointer)
     (name :pointer))
  (get-name (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
            name))

(defmethod create-instance ((self host-application) cid iid obj)
  (break)
  (cond ((and (uid-equal cid vst3-ffi::+vst-imessage-iid+)
              (uid-equal iid vst3-ffi::+vst-imessage-iid+))
         (let ((message (make-instance 'message)))
           (setf (cffi:mem-ref obj :pointer) (ptr message))
           (sb-ext:cancel-finalization message))
         sb:+k-result-true+)
        ((and (uid-equal cid vst3-ffi::+vst-iattribute-list-iid+)
              (uid-equal iid vst3-ffi::+vst-iattribute-list-iid+ ))
         (let ((attribute-list (make-instance 'attribute-list)))
           (setf (cffi:mem-ref obj :pointer) (ptr attribute-list))
           (sb-ext:cancel-finalization attribute-list))
         sb:+k-result-true+)
        (t   sb:+k-result-false+)))

(autowrap:defcallback create-instance sb:tresult
    ((this-interface :pointer)
     (cid :pointer)
     (iid :pointer)
     (obj :pointer))
  (create-instance (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                   cid iid obj))

(defclass component-handler (unknown)
  ((module :initarg :module :accessor .module)))

(defmethod query-interface ((self component-handler) iid obj)
  (%query-interface
   self vst3-ffi::+vst-icomponent-handler-iid+ iid obj
   (call-next-method)))

(defmethod initialize-instance :before ((self component-handler) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'sb:vst-i-component-handler-vtbl))
          (wrap (autowrap:alloc 'sb:vst-i-component-handler)))
      (setf (sb:vst-i-component-handler.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))
  (let ((vtbl (if (typep (.vtbl self) 'sb:vst-i-component-handler-vtbl)
                  (.vtbl self)
                  (sb::make-vst-i-component-handler-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (sb:vst-i-component-handler-vtbl.begin-edit vtbl)
          (autowrap:callback 'begin-edit))
    (setf (sb:vst-i-component-handler-vtbl.perform-edit vtbl)
          (autowrap:callback 'perform-edit))
    (setf (sb:vst-i-component-handler-vtbl.end-edit vtbl)
          (autowrap:callback 'end-edit))
    (setf (sb:vst-i-component-handler-vtbl.restart-component vtbl)
          (autowrap:callback 'restart-component))))

(defmethod begin-edit ((self component-handler) id)
  (dgw::begin-edit (.module self) id)
  sb:+k-result-ok+)

(autowrap:defcallback begin-edit sb:tresult
    ((this-interface :pointer)
     (id :unsigned-int))
  (begin-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
              id))

(defmethod perform-edit ((self component-handler) id value-normalized)
  (dgw::perform-edit (.module self) id value-normalized)
  sb:+k-result-ok+)

(autowrap:defcallback perform-edit sb:tresult
    ((this-interface :pointer)
     (id :unsigned-int)
     (value-normalized :double))
  (perform-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
               id value-normalized))

(defmethod end-edit ((self component-handler) id)
  (dgw::end-edit (.module self) id)
  sb:+k-result-ok+)

(autowrap:defcallback end-edit sb:tresult
    ((this-interface :pointer)
     (id :unsigned-int))
  (end-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
            id))

(defmethod restart-component ((self component-handler) flags)
  (dgw::restart-component (.module self) flags)
  sb:+k-result-ok+)

(autowrap:defcallback restart-component sb:tresult
    ((this-interface :pointer)
     (flags :int))
  (restart-component (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                     flags))

(defclass component-handler2 (component-handler)
  ((dirty :initform nil :accessor .dirty)))

(defmethod query-interface ((self component-handler2) iid obj)
  (%query-interface
   self vst3-ffi::+vst-icomponent-handler2-iid+ iid obj
   (call-next-method)))

(defmethod initialize-instance :before ((self component-handler2) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'sb:vst-i-component-handler2vtbl))
          (wrap (autowrap:alloc 'sb:vst-i-component-handler2)))
      (setf (sb:vst-i-component-handler2.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))
  (let ((vtbl (if (typep (.vtbl self) 'sb:vst-i-component-handler2vtbl)
                  (.vtbl self)
                  (sb::make-vst-i-component-handler2vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (sb:vst-i-component-handler2vtbl.set-dirty vtbl)
          (autowrap:callback 'set-dirty))
    (setf (sb:vst-i-component-handler2vtbl.request-open-editor vtbl)
          (autowrap:callback 'request-open-editor))
    (setf (sb:vst-i-component-handler2vtbl.start-group-edit vtbl)
          (autowrap:callback 'start-group-edit))
    (setf (sb:vst-i-component-handler2vtbl.finish-group-edit vtbl)
          (autowrap:callback 'finish-group-edit))))

(defmethod set-dirty ((self component-handler2) state)
  (setf (.dirty self) (/= state 0)))

(autowrap:defcallback set-dirty sb:tresult
    ((this-interface :pointer)
     (state sb:t-bool))
  (set-dirty (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
             state))

(defmethod request-open-editor ((self component-handler2) name)
  (declare (ignore name)))

(autowrap:defcallback request-open-editor sb:tresult
    ((this-interface :pointer)
     (name sb::fid-string))
  (request-open-editor (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                       name))

(defmethod start-group-edit ((self component-handler2)))

(autowrap:defcallback start-group-edit sb:tresult
    ((this-interface :pointer))
  (start-group-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))

(defmethod finish-group-edit ((self component-handler2)))

(autowrap:defcallback finish-group-edit sb:tresult
    ((this-interface :pointer))
  (finish-group-edit (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))

(defclass bstream (unknown)
  ((buffer :initform (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
           :reader .buffer)
   (cursor :initform 0 :accessor .cursor)))

(defmethod initialize-instance :before ((self bstream) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'sb:ib-stream-vtbl))
          (wrap (autowrap:alloc 'sb:ib-stream)))
      (setf (sb:ib-stream.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))

  (let ((vtbl (if (typep (.vtbl self) 'sb:ib-stream-vtbl)
                  (.vtbl self)
                  (sb::make-ib-stream-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (sb:ib-stream-vtbl.read vtbl)
          (autowrap:callback '.read))
    (setf (sb:ib-stream-vtbl.write vtbl)
          (autowrap:callback '.write))
    (setf (sb:ib-stream-vtbl.seek vtbl)
          (autowrap:callback 'seek))
    (setf (sb:ib-stream-vtbl.tell vtbl)
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
    sb:+k-result-ok+))

(autowrap:defcallback .read sb:tresult
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
  sb:+k-result-ok+)

(autowrap:defcallback .write sb:tresult
    ((this-interface :pointer)
     (buffer :pointer)
     (num-bytes :int)
     (num-bytes-written :pointer))
  (.write (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
          buffer num-bytes num-bytes-written))

(defmethod seek ((self bstream) pos mod result)
  (setf (.cursor self)
        (case mod
          (#.sb:+ib-stream-i-stream-seek-mode-k-ib-seek-set+
           (max pos (length (.buffer self))))
          (#.sb:+ib-stream-i-stream-seek-mode-k-ib-seek-cur+
           (max (+ pos (.cursor self)) (length (.buffer self))))
          (#.sb:+ib-stream-i-stream-seek-mode-k-ib-seek-end+
           (min 0 (- (length (.buffer self)) pos)))))
  (setf (cffi:mem-ref result :int64) (.cursor self))
  sb:+k-result-ok+)

(autowrap:defcallback seek sb:tresult
    ((this-interface :pointer)
     (pos :long-long)
     (mod :int)
     (result :pointer))
  (seek (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
          pos mod result))

(defmethod tell ((self bstream) pos)
  (setf (cffi:mem-ref pos :int64) (.cursor self))
  sb:+k-result-ok+)

(autowrap:defcallback tell sb:tresult
    ((this-interface :pointer)
     (pos :pointer))
  (tell (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
        pos))

(defclass message (unknown)
  ((message-id :initform nil :accessor .message-id)
   (attribute-list :initform nil :accessor .attribute-list)))

(defmethod initialize-instance :before ((self message) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'sb:vst-i-message-vtbl ))
          (wrap (autowrap:alloc 'sb:vst-i-message)))
      (setf (sb:vst-i-message.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))

  (let ((vtbl (if (typep (.vtbl self) 'sb:vst-i-message-vtbl)
                  (.vtbl self)
                  (sb::make-vst-i-message-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (sb:vst-i-message-vtbl.get-message-id vtbl)
          (autowrap:callback 'get-message-id))
    (setf (sb:vst-i-message-vtbl.set-message-id vtbl)
          (autowrap:callback '.set-message-id))
    (setf (sb:vst-i-message-vtbl.get-attributes vtbl)
          (autowrap:callback 'get-attributes))))

(defmethod get-message-id ((self message))
  (.message-id self))

(autowrap:defcallback get-message-id :pointer
    ((this-interface :pointer))
  (get-message-id (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))

(defmethod set-message-id ((self message) id)
  (when (.message-id self)
    (autowrap:free (.message-id self)))
  (let ((len 1))
    (loop for i to 1024
          until (zerop (autowrap:c-aref id i :char))
          do (incf len))
    (let ((ptr (autowrap:alloc :int8 len)))
      (loop for i below len
            do (setf (autowrap:c-aref ptr i :char)
                     (autowrap:c-aref id i :char)))
      (setf (.message-id self) ptr)))
  (values))

(autowrap:defcallback set-message-id :void
    ((this-interface :pointer)
     (id :pointer))
  (set-message-id (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                  id))

(defmethod get-attributes ((self message))
  (unless (.attribute-list self)
    (setf (.attribute-list self) (make-instance 'attribute-list)))
  (ptr (.attribute-list self)))

(autowrap:defcallback get-attributes :pointer
    ((this-interface :pointer))
  (get-attributes (gethash (cffi:pointer-address this-interface) *ptr-object-map*)))

(defclass attribute-list (unknown)
  ((map :initform (make-hash-table :test #'equal) :accessor .map)))

(defmethod initialize-instance :before ((self attribute-list) &key)
  (unless (slot-boundp self 'wrap)
    (let ((vtbl (autowrap:alloc 'sb:vst-i-attribute-list-vtbl))
          (wrap (autowrap:alloc 'sb:vst-i-attribute-list)))
      (setf (sb:vst-i-attribute-list.lp-vtbl wrap)
            (autowrap:ptr vtbl))
      (setf (slot-value self 'wrap) wrap)
      (setf (slot-value self 'vtbl) vtbl)))

  (let ((vtbl (if (typep (.vtbl self) 'sb:vst-i-attribute-list-vtbl)
                  (.vtbl self)
                  (sb::make-vst-i-attribute-list-vtbl
                   :ptr (autowrap:ptr (.vtbl self))))))
    (setf (sb:vst-i-attribute-list-vtbl.set-int vtbl)
          (autowrap:callback 'set-int))
    (setf (sb:vst-i-attribute-list-vtbl.get-int vtbl)
          (autowrap:callback 'get-int))
    (setf (sb:vst-i-attribute-list-vtbl.set-float vtbl)
          (autowrap:callback 'set-float))
    (setf (sb:vst-i-attribute-list-vtbl.get-float vtbl)
          (autowrap:callback 'get-float))
    (setf (sb:vst-i-attribute-list-vtbl.set-string vtbl)
          (autowrap:callback 'set-string))
    (setf (sb:vst-i-attribute-list-vtbl.get-string vtbl)
          (autowrap:callback 'get-string))
    (setf (sb:vst-i-attribute-list-vtbl.set-binary vtbl)
          (autowrap:callback 'set-binary))
    (setf (sb:vst-i-attribute-list-vtbl.get-binary vtbl)
          (autowrap:callback 'get-binary))))

(defmethod set-int ((self attribute-list) id value)
  (setf (gethash id (.map self)) value)
  sb:+k-result-true+)

(autowrap:defcallback set-int :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (value sb:int64))
  (set-int (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
           id value))

(defmethod get-int ((self attribute-list) id value)
  (let ((x (gethash id (.map self))))
    (if (typep x 'integer)
        (progn
          (setf (cffi:mem-ref value :long-long) x)
          sb:+k-result-true+)
        sb:+k-result-false+)))

(autowrap:defcallback get-int :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (value (:pointer sb:int64)))
  (get-int (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
           id value))

(defmethod set-float ((self attribute-list) id value)
  (setf (gethash id (.map self)) value)
  sb:+k-result-true+)

(autowrap:defcallback set-float :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (value :double))
  (set-float (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
           id value))

(defmethod get-float ((self attribute-list) id value)
  (let ((x (gethash id (.map self))))
    (if (typep x 'double-float)
        (progn
          (setf (cffi:mem-ref value :double) x)
          sb:+k-result-true+)
        sb:+k-result-false+)))

(autowrap:defcallback get-float :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (value (:pointer :double)))
  (get-float (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
           id value))

(defmethod set-string ((self attribute-list) id string)
  (let ((len 1))
    (loop for i from 0
          until (zerop (cffi:mem-ref string :int16 i))
          do (incf len))
    (let ((v (make-array (* len 2) :element-type '(unsigned-byte 8))))
      (loop for i below (* len 2)
            do (setf (cffi:mem-ref string :char i)
                     (aref v i)))
      (let ((s (sb-ext:octets-to-string v :external-format :utf16le)))
        (setf (gethash id (.map self)) s)))
    sb:+k-result-true+))

(autowrap:defcallback set-string :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (string (:pointer sb:vst-t-char)))
  (set-string (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
           id string))

(defmethod get-string ((self attribute-list) id string size-in-bytes)
  (let ((x (gethash id (.map self))))
    (if (typep x 'string)
        (let* ((v (sb-ext:string-to-octets x :external-format :utf16le))
               (len (min (- size-in-bytes 2) (* (length x) 2))))
          (loop for i below (* len 2)
                do (setf (cffi:mem-ref string :char)
                         (aref v i)))
          (setf (cffi:mem-ref string :int16 len) 0)
          sb:+k-result-true+)
        sb:+k-result-false+)))

(autowrap:defcallback get-string :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (string (:pointer sb:vst-t-char))
     (size-in-bytes sb:uint32))
  (get-string (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
              id string size-in-bytes))

(defmethod set-binary ((self attribute-list) id data size-in-bytes)
  (let ((v (make-array size-in-bytes :element-type '(unsigned-byte 8))))
    (loop for i below size-in-bytes
          do (setf (aref v i)
                   (cffi:mem-ref data :unsigned-char i)))
    (setf (gethash id (.map self)) v))
  sb:+k-result-true+)

(autowrap:defcallback set-binary :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (data :pointer)
     (size-in-bytes sb:uint32))
  (set-binary (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
           id data size-in-bytes))

(defmethod get-binary ((self attribute-list) id data size-in-bytes)
  (let ((x (gethash id (.map self))))
    (if (typep x '(simple-array (unsigned-byte 8)))
        (progn
          ;; TODO GC
          (setf (cffi:mem-ref data :pointer) (sb-sys:vector-sap x))
          (setf (cffi:mem-ref data :unsigned-int) (length x))
          sb:+k-result-true+)
        sb:+k-result-false+)))

(autowrap:defcallback get-binary :pointer
    ((this-interface :pointer)
     (id (:pointer :char))
     (data :pointer)
     (size-in-bytes (:pointer sb:uint32)))
  (get-binary (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
           id data size-in-bytes))


(defmacro def-vst3-impl (name super-classes slots methods
                         &key iid vst3-c-api-class)
  (labels ((sym (format &rest args)
             (find-symbol (apply #'format nil format args) :sb)))
    `(progn
       (defclass ,name ,super-classes
         ,slots)

       (defmethod query-interface ((self ,name) iid obj)
         (%query-interface self ,iid iid obj
                           (call-next-method)))

       (defmethod initialize-instance :before ((self ,name) &key)
         (unless (slot-boundp self 'wrap)
           (let ((vtbl (autowrap:alloc ',(sym "~a-VTBL" vst3-c-api-class)))
                 (wrap (autowrap:alloc ',vst3-c-api-class)))
             (setf (,(sym "~a.LP-VTBL" vst3-c-api-class) wrap)
                   (autowrap:ptr vtbl))
             (setf (slot-value self 'wrap) wrap)
             (setf (slot-value self 'vtbl) vtbl)))

         (let ((vtbl (if (typep (.vtbl self) ',(sym "~a-VTBL" vst3-c-api-class))
                         (.vtbl self)
                         (,(sym "MAKE-~a-VTBL" vst3-c-api-class)
                          :ptr (autowrap:ptr (.vtbl self))))))
           ,@(loop for method in methods
                   for method-name = (car method)
                   collect `(setf (,(sym "~a-VTBL.~a" vst3-c-api-class method-name) vtbl)
                                  (autowrap:callback ',method-name)))))

       ,@(loop for method in methods
               for (method-name method-args result-type . body) = method
               collect `(defmethod ,method-name ((self ,name) ,@(mapcar #'car method-args))
                          ,@body)
               collect `(autowrap:defcallback ,method-name ,result-type
                            ((this-interface :pointer)
                             ,@method-args)
                          (,method-name (gethash (cffi:pointer-address this-interface) *ptr-object-map*)
                                        ,@(mapcar #'car method-args)))))))


(def-vst3-impl plug-frame (unknown)
  ((module :initarg :module :accessor .module))
  ((resize-view ((view (:pointer sb:i-plug-view))
                 (new-size (:pointer (:struct (sb:view-rect)))))
                sb:tresult
                (declare (ignore view))
                ;; TODO 実装
                (let ((hwnd (dgw::.hwnd (.module self)))
                      (width (- (plus-c:c-ref new-size (:struct (sb:view-rect)) :right)
                                (plus-c:c-ref new-size (:struct (sb:view-rect)) :left)))
                      (heigth (- (plus-c:c-ref new-size (:struct (sb:view-rect)) :bottom)
                                 (plus-c:c-ref new-size (:struct (sb:view-rect)) :top))))
                  (win32::resize hwnd width heigth))
                sb:+k-result-true+))
  :iid vst3-ffi::+iplug-frame-iid+
  :vst3-c-api-class sb:i-plug-frame)

(def-vst3-impl parameter-changes (unknown)
  ((changes :initform nil :accessor .changes))
  ((get-parameter-count ()
                        :int
                        ;; TODO
                        0)
   (get-parameter-data ((id (:pointer :unsigned-int))
                        (index (:pointer :int)))
                       :pointer
                       ;; TODO
                       (cffi:null-pointer))
   (add-parameter-data ((index :int))
                       :pointer
                       ;; TODO
                       (cffi:null-pointer)))
  :iid vst3-ffi::+vst-iparameter-changes-iid+
  :vst3-c-api-class sb:vst-i-parameter-changes)

(defmethod dgw::prepare ((self parameter-changes))
  (loop for change in (.changes self)
        do (autowrap:free change)))

(defmethod release :around ((self parameter-changes))
  (when (zerop (call-next-method))
    (dgw::prepare self)))

(def-vst3-impl event-list (unknown)
  ((events :initform nil :accessor .events))
  ((get-event-count ()
                    :int
                    (length (.events self)))
   (get-event ((index :int)
               (e :pointer))
              sb:tresult
              (dgw::memcpy e
                           (autowrap:ptr (nth index (.events self)))
                           (autowrap:sizeof '(:struct (sb:vst-event))))
              sb:+k-result-ok+)
   (add-event ((e :pointer))
              sb:tresult
              (let ((event (autowrap:alloc '(:struct (sb:vst-event)))))
                (dgw::memcpy (autowrap:ptr event)
                             e
                             (autowrap:sizeof '(:struct (sb:vst-event)))))
              sb:+k-result-ok+))
  :iid vst3-ffi::+vst-ievent-list-iid+
  :vst3-c-api-class sb:vst-i-event-list)

(defmethod dgw::prepare ((self event-list))
  (loop for event in (.events self)
        do (autowrap:free event)))

(defmethod release :around ((self event-list))
  (when (zerop (call-next-method))
    (dgw::prepare self)))

(in-package :dd-ffi)

;;; https://yohhoy.hatenablog.jp/entry/2012/07/05/211940
;;; https://gist.github.com/yohhoy/3053385

(defconstant +ok+ 0)
(defconstant +no-interface+ -2147467262)

(defvar *ptr-object-map* (make-hash-table :weakness :value))

(cffi:defcstruct i-unknown-vtbl
  (query-interface :pointer)
  (add-ref :pointer)
  (release :pointer))

;;; C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\um\oleidl.h
(cffi:defcstruct i-drop-target-vtbl
  (query-interface :pointer)
  (add-ref :pointer)
  (release :pointer)
  (drag-enter :pointer)
  (drag-over :pointer)
  (drag-leave :pointer)
  (drop :pointer))

;;; C:\Program Files (x86)\Windows Kits\10\Include\10.0.19041.0\um\ObjIdl.h
(cffi:defcstruct i-data-object-vtbl
  (query-interface :pointer)
  (add-ref :pointer)
  (release :pointer)
  (get-data :pointer)
  (get-data-here :pointer)
  (query-get-data :pointer)
  (get-canonical-format-etc :pointer)
  (set-data :pointer)
  (enum-format-etc :pointer)
  (d-advice :pointer)
  (d-unadvice :pointer)
  (enum-d-advice :pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun iid-from-string (str)
    (let ((iid 0)
          (integer (parse-integer (delete #\- str) :radix 16)))
      (setf (ldb (byte 8   0) iid) (ldb (byte 8 120) integer))
      (setf (ldb (byte 8   8) iid) (ldb (byte 8 112) integer))
      (setf (ldb (byte 8  16) iid) (ldb (byte 8 104) integer))
      (setf (ldb (byte 8  24) iid) (ldb (byte 8  96) integer))
      (setf (ldb (byte 8  32) iid) (ldb (byte 8  72) integer))
      (setf (ldb (byte 8  40) iid) (ldb (byte 8  64) integer))
      (setf (ldb (byte 8  48) iid) (ldb (byte 8  88) integer))
      (setf (ldb (byte 8  56) iid) (ldb (byte 8  80) integer))
      (setf (ldb (byte 8  64) iid) (ldb (byte 8  32) integer))
      (setf (ldb (byte 8  72) iid) (ldb (byte 8  40) integer))
      (setf (ldb (byte 8  80) iid) (ldb (byte 8  48) integer))
      (setf (ldb (byte 8  88) iid) (ldb (byte 8  56) integer))
      (setf (ldb (byte 8  96) iid) (ldb (byte 8   0) integer))
      (setf (ldb (byte 8 104) iid) (ldb (byte 8   8) integer))
      (setf (ldb (byte 8 112) iid) (ldb (byte 8  16) integer))
      (setf (ldb (byte 8 120) iid) (ldb (byte 8  24) integer))
      iid))

  (defun iid-from-alien (pointer)
    (let ((iid 0))
      (setf (ldb (byte 8   0) iid) (cffi:mem-aref pointer :unsigned-char 15))
      (setf (ldb (byte 8   8) iid) (cffi:mem-aref pointer :unsigned-char 14))
      (setf (ldb (byte 8  16) iid) (cffi:mem-aref pointer :unsigned-char 13))
      (setf (ldb (byte 8  24) iid) (cffi:mem-aref pointer :unsigned-char 12))
      (setf (ldb (byte 8  32) iid) (cffi:mem-aref pointer :unsigned-char  9))
      (setf (ldb (byte 8  40) iid) (cffi:mem-aref pointer :unsigned-char  8))
      (setf (ldb (byte 8  48) iid) (cffi:mem-aref pointer :unsigned-char 11))
      (setf (ldb (byte 8  56) iid) (cffi:mem-aref pointer :unsigned-char 10))
      (setf (ldb (byte 8  64) iid) (cffi:mem-aref pointer :unsigned-char  4))
      (setf (ldb (byte 8  72) iid) (cffi:mem-aref pointer :unsigned-char  5))
      (setf (ldb (byte 8  80) iid) (cffi:mem-aref pointer :unsigned-char  6))
      (setf (ldb (byte 8  88) iid) (cffi:mem-aref pointer :unsigned-char  7))
      (setf (ldb (byte 8  96) iid) (cffi:mem-aref pointer :unsigned-char  0))
      (setf (ldb (byte 8 104) iid) (cffi:mem-aref pointer :unsigned-char  1))
      (setf (ldb (byte 8 112) iid) (cffi:mem-aref pointer :unsigned-char  2))
      (setf (ldb (byte 8 120) iid) (cffi:mem-aref pointer :unsigned-char  3))
      iid))

  (defmacro define-query-interface (class str)
    (let ((iid-symbol (intern (format nil "+~a-iid+" class)))
          (iid-value (iid-from-string str)))
      `(progn
         (defconstant ,iid-symbol ,iid-value)
         (defmethod query-interface ((,class ,class) iid object)
           (break "query-interface ~a ~a ~a" ,class iid object)
           (if (= (iid-from-alien iid) ,iid-value)
               (progn
                 (setf (cffi:mem-ref object :pointer)
                       (.vtbl ,class))
                 +ok+)
               (call-next-method))
           )))))

(defclass unknown ()
  ((vtbl :accessor .vtbl)
   (ref-count :initform 1 :accessor .ref-count)))

(defmethod initialize-instance :after ((unknown unknown) &key)
  (setf (gethash (cffi:pointer-address (.vtbl unknown))
                 *ptr-object-map*)
        unknown))

(defmethod initialize-instance :before ((unknown unknown) &key)
  (unless (slot-boundp unknown 'vtbl)
    (setf (.vtbl unknown)
          (cffi:foreign-alloc '(:struct i-unknown-vtbl))))
  (cffi:with-foreign-slots ((query-interface add-ref release)
                            (.vtbl unknown) (:struct i-unknown-vtbl))
    (setf query-interface (cffi:callback query-interface))
    (setf add-ref (cffi:callback add-ref))
    (setf release (cffi:callback release))))

(define-query-interface unknown "00000000-0000-0000-C000-000000000046")

(defmethod query-interface (x iid object)
  (setf (cffi:mem-ref object :pointer) (cffi:null-pointer))
  +no-interface+)

(cffi:defcallback query-interface :long
    ((this :pointer)
     (iid :pointer)
     (object :pointer))
  (query-interface (gethash (cffi:pointer-address this) *ptr-object-map*)
                   iid object))

(defmethod add-ref ((unknown unknown))
  (incf (.ref-count unknown)))

(cffi:defcallback add-ref :unsigned-long
    ((this :pointer))
  (add-ref (gethash (cffi:pointer-address this) *ptr-object-map*)))

(defmethod release ((unknown unknown))
  (let ((ref-count (decf (.ref-count unknown))))
    (when (zerop ref-count)
      (cffi:foreign-free (.vtbl unknown))
      (setf (.vtbl unknown) nil))
    ref-count))

(cffi:defcallback release :unsigned-long
    ((this :pointer))
  (release (gethash (cffi:pointer-address this) *ptr-object-map*)))


(defclass drop-target (unknown)
  ())

(define-query-interface drop-target "00000122-0000-0000-C000-000000000046")

(defmethod initialize-instance :before ((drop-target drop-target) &key)
  (unless (slot-boundp drop-target 'vtbl)
    (setf (.vtbl drop-target)
          (cffi:foreign-alloc '(:struct i-drop-target-vtbl))))
  (cffi:with-foreign-slots ((drag-enter drag-over drag-leave drop)
                            (.vtbl drop-target) (:struct i-drop-target-vtbl))
    (setf drag-enter (cffi:callback drag-enter))
    (setf drag-over (cffi:callback drag-over))
    (setf drag-leave (cffi:callback drag-leave))
    (setf drop (cffi:callback drop))))

(defconstant +max-path+ 260)
(defconstant +dropeffect-none+ 0)
(defconstant +dropeffect-copy+ 1)
(defconstant +dropeffect-move+ 2)
(defconstant +dropeffect-link+ 4)

(defconstant +cf-hdrop+ 15)

(cffi:defcstruct formatetc
  (format :unsigned-short)
  (ptd :pointer)
  (aspect :unsigned-long)
  (index :long)
  (tymed :unsigned-long))

(cffi:defcunion stgmedium-union
  (h-bitmap :pointer)
  (h-meta-file-pict :pointer)
  (h-enh-meta-file :pointer)
  (h-global :pointer)
  (lpsz-file-name :pointer)
  (pstm :pointer)
  (pstg :pointer))

(cffi:defcstruct stgmedium
  (tymed :unsigned-long)
  (union (:union stgmedium-union))
  (p-unk-for-release :pointer))

(cffi:defcfun ("GlobalLock" global-lock) :pointer
  (h-mem :pointer))

(cffi:defcfun ("GlobalUnlock" global-unlock) :int
  (h-mem :pointer))

(cffi:defcfun ("DragQueryFileW" drag-query-file) :unsigned-int
  (h-drop :pointer)
  (i-file :unsigned-int)
  (lpsz-file :pointer)
  (cch :unsigned-int))

(cffi:defcfun ("ReleaseStgMedium" release-stg-medium) :void
  (lp-stgmedium :pointer))

(defmethod drag-enter ((drop-target drop-target) data state pt effect)
  (setf (cffi:mem-ref effect :unsigned-long) +dropeffect-none+)
  (cffi:with-foreign-objects ((formatetc '(:struct formatetc))
                              (stgmedium '(:struct stgmedium)))
    (cffi:with-foreign-slots ((format ptd aspect index tymed) formatetc (:struct formatetc))
      (setf format +cf-hdrop+)
      (setf ptd (cffi:null-pointer))
      (setf aspect 1)                   ;DVASPECT_CONTENT
      (setf index -1)
      (setf tymed 1))                   ;TYMED_HGLOBAL
    (unless (minusp (cffi:foreign-funcall-pointer
                     (cffi:foreign-slot-value data '(:struct i-data-object-vtbl) 'get-data) ()
                     :pointer formatetc
                     :pointer stgmedium
                     :long))
      (let ((h-drop (global-lock (cffi:foreign-slot-value stgmedium '(:struct stgmedium) 'union))))
        (unless (cffi:null-pointer-p h-drop)
          (let ((files
                  (loop with file-path = (make-array (* +max-path+ 2) :element-type '(unsigned-byte 8))
                        with file-count = (drag-query-file h-drop #xFFFFFFFF (cffi:null-pointer) 0)
                        for i below file-count
                        for file = (sb-sys:with-pinned-objects (file-path)
                                     (and (plusp (drag-query-file
                                                  h-drop i (sb-sys:vector-sap file-path) +max-path+))
                                          (sb-ext:octets-to-string file-path
                                                                   :external-format :utf16le)))
                        if (and file (equalp (pathname-type file) "wav"))
                          collect file)))
            (when files
              (setf (cffi:mem-ref effect :unsigned-long) +dropeffect-copy+)
              (dgw::drag-enter dgw::*app* files)))))
      (global-unlock (cffi:foreign-slot-value stgmedium '(:struct stgmedium) 'union))
      (release-stg-medium stgmedium)))
  +ok+)

(cffi:defcallback drag-enter :long
    ((this :pointer)
     (data :pointer)
     (state :unsigned-long)
     (pt :pointer)
     (effect :pointer))
  (drag-enter (gethash (cffi:pointer-address this) *ptr-object-map*)
              data state pt effect))

(defmethod drag-over ((drop-target drop-target) state pt effect)
  (setf (cffi:mem-ref effect :unsigned-long) +dropeffect-copy+)
  +ok+)

(cffi:defcallback drag-over :long
    ((this :pointer)
     (state :unsigned-long)
     (pt :pointer)
     (effect :pointer))
  (drag-over (gethash (cffi:pointer-address this) *ptr-object-map*)
             state pt effect))

(defmethod drag-leave ((drop-target drop-target))
  +ok+)

(cffi:defcallback drag-leave :long
    ((this :pointer))
  (drag-leave (gethash (cffi:pointer-address this) *ptr-object-map*)))

(defmethod drop ((drop-target drop-target) data state pt effect)
  (dgw::drop dgw::*app*)
  (setf (cffi:mem-ref effect :unsigned-long) +dropeffect-copy+)
  +ok+)

(cffi:defcallback drop :long
    ((this :pointer)
     (data :pointer)
     (state :unsigned-long)
     (pt :pointer)
     (effect :pointer))
  (drop (gethash (cffi:pointer-address this) *ptr-object-map*)
        data state pt effect))

;;; WINOLEAPI  RegisterDragDrop(IN HWND hwnd, IN LPDROPTARGET pDropTarget)
(cffi:defcfun ("RegisterDragDrop" register-drag-drop) :long
  (hwnd :pointer)
  (p-drop-target :pointer))

;;; WINOLEAPI  RevokeDragDrop(IN HWND hwnd);
(cffi:defcfun ("RevokeDragDrop" revoke-drag-drop) :long
  (hwnd :pointer))

(defmacro with-drag-drop-handler ((hwnd) &body body)
  (let ((drop-target (gensym "DROP-TARGET")))
    `(let ((,drop-target (make-instance 'drop-target)))
       (register-drag-drop ,hwnd (.vtbl ,drop-target))
       (unwind-protect (progn ,@body)
         (revoke-drag-drop ,hwnd)
         (release ,drop-target)))))

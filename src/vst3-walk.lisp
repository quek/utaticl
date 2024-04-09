(declaim (optimize (debug 3)))

(in-package :vst3-walk)

(defvar *vst3-c-api-h* (asdf:system-relative-pathname :dgw "lib/vst3_c_api/vst3_c_api.h"))
(defvar *h* (grovel::read-h *vst3-c-api-h*))


(cffi:defctype steinberg-tresult :int32)
(cffi:defctype steinberg-uint32 :int32)


(defun seek-to-interfaces (h)
  (loop for (x . xs) on h
          thereis  (and (eq :comment (car x))
                        (search "----- Interfaces -----" (cadr x))
                        xs)))

(defun parse-uid (a b c d)
  (let ((uid (make-array 16 :element-type '(unsigned-byte 8))))
    (setf (aref uid 0) (ldb (byte 8 0) a))
    (setf (aref uid 1) (ldb (byte 8 8) a))
    (setf (aref uid 2) (ldb (byte 8 16) a))
    (setf (aref uid 3) (ldb (byte 8 24) a))
    (setf (aref uid 4) (ldb (byte 8 16) b))
    (setf (aref uid 5) (ldb (byte 8 24) b))
    (setf (aref uid 6) (ldb (byte 8 0) b))
    (setf (aref uid 7) (ldb (byte 8 8) b))
    (setf (aref uid 8) (ldb (byte 8 24) c))
    (setf (aref uid 9) (ldb (byte 8 16) c))
    (setf (aref uid 10) (ldb (byte 8 8) c))
    (setf (aref uid 11) (ldb (byte 8 0) c))
    (setf (aref uid 12) (ldb (byte 8 24) d))
    (setf (aref uid 13) (ldb (byte 8 16) d))
    (setf (aref uid 14) (ldb (byte 8 8) d))
    (setf (aref uid 15) (ldb (byte 8 0) d))
    uid))

(defun lisp-name (symbol)
  (intern
   (with-output-to-string (out)
     (loop with suppress-hyphen = t
           for c across (string symbol)
           do (cond ((upper-case-p c)
                     (unless suppress-hyphen
                       (write-char #\- out))
                     (setf suppress-hyphen t)
                     (write-char (char-upcase c) out))
                    ((eql #\_ c)
                     (setf suppress-hyphen t)
                     (write-char #\- out))
                    (t
                     (setf suppress-hyphen nil)
                     (write-char (char-upcase c) out)))))))

(defun ffi-type (symbol)
  (cond ((string-equal symbol "double") :double)
        ((string-equal symbol "float") :float)
        ((string-equal symbol "void") :void)
        (t (autowrap:basic-foreign-type
            (autowrap:find-type
             (find-symbol
              (autowrap:default-c-to-lisp (symbol-name symbol))
              :vst3-c-api))))))

(defun split-by-comma (list)
  (let ((ys ()))
    (loop with y = nil
          for x in list
          if (string-equal "," x)
            do (push (nreverse y) ys)
               (setf y nil)
          else
            do (push x y)
          finally (when y (push (nreverse y) ys)))
    (nreverse ys)))

(defun parse-method-args (args)
  (loop for arg in (cdr (split-by-comma args))
        collect (lisp-name (car (last arg)))))

(defun parse-c-call-args (args)
  (loop for arg in (cdr (split-by-comma args))
        for var = (lisp-name (car (last arg)))
        nconc (let ((xs (remove "const" (butlast arg) :test #'string-equal)))
                     (cond ((string-equal "*" (car (last xs)))
                            `(:pointer ,var))
                           ((equalp xs '(read-vst3-c-api-h::|Steinberg_TUID|))
                            `(:pointer (sb-sys:vector-sap ,var)))
                           ((equalp xs '(read-vst3-c-api-h::|Steinberg_FIDString|))
                            `(:string ,var))
                           (t `(,(ffi-type (car xs)) ,var))))))

(defun find-vst3-c-api-symbol (class-name method-name)
  (find-symbol
   (format nil "~a.LP-VTBL*.~a"
           (autowrap:default-c-to-lisp (symbol-name class-name))
           (autowrap:default-c-to-lisp (symbol-name method-name)))
   :vst3-c-api))

(defun parse-field-result-type (exp)
  (let ((xs (loop for x in exp
                  if (consp x)
                    do (loop-finish)
                  unless (or (string-equal "const" x)
                             (string-equal "struct" x))
                    collect x)))
    (cond ((string-equal "*" (car (last xs)))
           :pointer)
          (t (ffi-type (car xs))))))

(defun method-name (field)
  (lisp-name (car (last (find-if #'consp field)))))

(defun parse-field (class-name field)
  (let* ((method-name (method-name field))
         (method-args (parse-method-args (car (last field))))
         (c-call-args (parse-c-call-args (car (last field))))
         (f (find-vst3-c-api-symbol class-name (car (last (find-if #'consp field)))))
         (result-type (parse-field-result-type field)))
    `(defmethod ,method-name ((self ,(lisp-name class-name)) ,@method-args)
       (cffi:foreign-funcall-pointer
        (,f (.instance self)) ()
        :pointer (.ptr self)
        ,@c-call-args
        ,result-type))))

(defclass %funknown ()
  ((ptr :initarg :ptr :reader .ptr)
   (instance :reader .instance)))

(defmethod initialize-instance :after ((self %funknown) &key ptr)
  (let ((release (vst3-c-api::steinberg-f-unknown.lp-vtbl*.release
                  (vst3-c-api::make-steinberg-f-unknown :ptr ptr)))
        (class (type-of self)))
    (sb-ext:finalize self
                     (lambda ()
                       (print (list 'finalize class ptr))
                       (print
                        (cffi::foreign-funcall-pointer
                         release ()
                         :pointer ptr
                         :uint32))))))

(defun def-vst3-interface (comment vtbl interface iid)
  (let* ((c-class (caddr interface))
         (bases (or (last (loop for field in (cadddr vtbl)
                                if (and (eq :comment (car field))
                                        (search "/* methods derived from \"" (cadr field)))
                                  collect (let* ((comment (cadr field)))
                                            (lisp-name (subseq comment
                                                               (1+ (position #\" comment))
                                                               (position #\" comment :from-end t))))))
                    '(%funknown)))
         (autowrap-lisp-class (autowrap:default-c-to-lisp (symbol-name c-class)))
         (class-name (lisp-name c-class))
         (iid-symbol (intern (format nil "+~a+" (lisp-name (nth 3 iid))))))
    `(progn
       (defvar *iid-class-map* (make-hash-table))

       (defclass ,class-name ,bases
         ()
         (:documentation ,(subseq (cadr comment)
                                  (position #\S (cadr comment)))))

       (defmethod initialize-instance :after ((self ,class-name) &key ptr)
         (setf (slot-value self 'instance)
               (,(find-symbol
                  (format nil "MAKE-~a" autowrap-lisp-class)
                  :vst3-c-api) :ptr ptr)))
       
       ,@(loop for field in (cadddr vtbl)
               unless (eq :comment (and (consp field) (car field)))
                 collect (parse-field c-class field))

       (alexandria:define-constant ,iid-symbol
           (parse-uid ,@(remove 'grovel::|,| (nth 6 iid)))
         :test #'equalp)

       (setf (gethash ,iid-symbol *iid-class-map*) ',class-name))))

(defmacro def-vst3-interfaces ()
  `(progn
     ,@(loop for (comment vtbl interface iid) on (seek-to-interfaces *h*) by #'cddddr
             collect (def-vst3-interface comment vtbl interface iid))))

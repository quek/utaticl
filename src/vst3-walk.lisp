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
           for c across (symbol-name symbol)
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
  (cond ((string-equal symbol "float") :float)
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
                     (cond ((string-equal "*" (print (car (last xs))))
                            `(:pointer ,var))
                           ((equalp xs '(read-vst3-c-api-h::|Steinberg_TUID|))
                            `(:pointer (sb-sys:vector-sap ,var)))
                           (t `(,(ffi-type (car xs)) ,var))))))

(defun find-vst3-c-api-symbol (class-name method-name)
  (find-symbol
   (format nil "~a.LP-VTBL*.~a"
           (autowrap:default-c-to-lisp (symbol-name class-name))
           (autowrap:default-c-to-lisp (symbol-name method-name)))
   :vst3-c-api))

(defun parse-field (class-name field)
  (let* ((method-name (lisp-name (car (last (find-if #'consp field)))))
         (method-args (parse-method-args (car (last field))))
         (c-call-args (parse-c-call-args (car (last field))))
         (f (find-vst3-c-api-symbol class-name method-name))

         (result-type (ffi-type (car field))))
    `(defmethod ,method-name ((self ,(lisp-name class-name)) ,@method-args)
       (cffi:foreign-funcall-pointer
        (,f (.instance self)) ()
        :pointer (.instance self)
        ,@c-call-args
        ,result-type))))

(defclass %funknown ()
  ((instance :initarg :instance :accessor .instance)))

(defun def-vst3-interface (comment vtbl interface iid)
  (let ((class-name (lisp-name (caddr interface))))
    `(progn
       (defclass ,class-name (%funknown)
         ()
         (:documentation ,(subseq (cadr comment)
                                  (position #\S (cadr comment)))))
       ,@(loop for field in (cadddr vtbl)
               unless (eq :comment (and (consp field) (car field)))
                 collect (parse-field (caddr interface) field))
       (alexandria:define-constant ,(intern (format nil "+~a+" (lisp-name (nth 3 iid))))
           (parse-uid ,@(remove 'grovel::|,| (nth 6 iid)))
         :test #'equalp))))

(defmacro def-vst3-interfaces ()
  `(progn
     ,@(loop for (comment vtbl interface iid) on (seek-to-interfaces *h*) by #'cddddr
             collect (def-vst3-interface comment vtbl interface iid))))

#+nil
(def-vst3-interfaces)

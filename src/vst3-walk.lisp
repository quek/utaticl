(in-package :vst3-walk)

(defvar *vst3-c-api-h* (asdf:system-relative-pathname :dgw "lib/vst3_c_api/vst3_c_api.h"))
(defvar *h* (grovel::read-h *vst3-c-api-h*))

(defun seek-to-interfaces (h)
  (loop for (x . xs) on h
          thereis  (and (eq :comment (car x))
                        (search "----- Interfaces -----" (cadr x))
                        xs)))


(defun parse-uid (args)
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
             bytes))
         (parse-args (arg)
           (parse-integer (symbol-name arg) :start 2 :radix 16)))
    (let ((tuid (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
          (args (mapcar #'parse-args args)))
      (loop for i from 0 below 4
            do (let ((bytes (funcall (case i
                                       (0 #'bytes1)
                                       (1 #'bytes2)
                                       (t #'bytes34))
                                     (nth i args))))
                 (loop for j from 0 below 4
                       do (setf (aref tuid (+ (* i 4) j)) (aref bytes j)))))
      tuid)))


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

(defun parse-field (class-name field)
  (let* ((method-name (lisp-name (car (last (find-if #'consp field)))))
         (args (car (last field)))
         (f (find-symbol (format nil "~a-~a" class-name method-name)))
         (result-type (car field)))
    `(defmethod ,method-name ((self ,class-name) ,@args)
         (cffi:foreign-funcall-pointer
          (,f (.instance self) ,@args ,result-type)))))

(defun def-vst3-interface (comment vtbl interface iid)
  (let ((class-name (lisp-name (caddr interface)))
        #+nil
        (bases (loop for field in (cadddr vtbl)
                     if (and (eq :comment (car (print field)))
                             (search "/* methods derived from \"" (cadr field)))
                       collect (let* ((comment (cadr field)))
                                 (lisp-name (subseq comment
                                                    (1+ (position #\" comment))
                                                    (position #\" comment :from-end t)))))))
    `(progn
       (defclass ,class-name ()
         (instance :initarg :instance :accessor .instance)
         ((:docuemnt ,(subseq (cadr comment)
                              (position #\S (cadr comment))))))
       ,@(loop for field in (cadddr vtbl)
               unless (eq :comment (and (consp field) (car field)))
                 collect (parse-field class-name field))
       (defconstant ,(lisp-name (nth 3 iid)) (parse-uid ',(nth 6 iid))))))

(defmacro def-vst3-interfaces ()
  `(progn
     ,@(loop for (comment vtbl interface iid) on (seek-to-interfaces *h*) by #'cddddr
             collect (def-vst3-interface comment vtbl interface iid))))

#+nil
(def-vst3-interfaces)

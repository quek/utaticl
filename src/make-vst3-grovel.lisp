(in-package :grovel)

(cffi:defctype read-vst3-c-api-h::char :char)
(cffi:defctype char16 :int16)
(cffi:defctype vst3::char16_t :int16)
(cffi:defctype vst3::int32_t :int32)
(cffi:defctype vst3::uint32_t :uint32)

(defvar *vst3-c-api-h* (asdf:system-relative-pathname :dgw "lib/vst3_c_api/vst3_c_api.h"))

(defun extract-name (list)
  (let ((name (car (last list))))
    (if (consp name)
        (nth (- (length list) 2) list)
        name)))

(defun name-c (name)
  (let ((name (string name)))
    (subseq name 0 (position #\[ name))))

(defun name-lisp (name)
  (let* ((name (name-c name))
         (pos (position #\_ name :from-end t))
         (name (if pos
                   (subseq name (1+ pos))
                   name)))
    (intern (with-output-to-string (out)
       (loop for c across name
             for first = t then nil
             if (and (not first) (upper-case-p c))
               do (format out "-~c" (char-upcase c))
             else
               do (write-char (char-upcase c) out)))
            :vst3)))

(defgeneric emit (x stream)
  (:method (x stream)
    (prin1 x stream))
  (:method ((x symbol) stream)
    (when (keywordp x)
        (write-char #\: stream))
    (princ (string-downcase (symbol-name x)) stream))
  (:method ((x list) stream)
    (write-char #\( stream)
    (loop for i in x
          for space = nil then #\space
          if space
            do (write-char space stream)
          do (emit i stream))
    (write-char #\) stream)))

(defvar *h* nil)

(setf *h*
      (with-open-file (in *vst3-c-api-h*)
        (let ((*readtable* (make-readtable t nil))
              (*package* (find-package :read-vst3-c-api-h))
              (*statement* nil))
          (loop for x = (read in nil in)
                until (eq x in)
                collect x))))

(defun grovel-type (slot)
  (let ((type (gethash (name-c (car slot)) *vst3-grovel-types*))
        (count (car (last slot))))
    (if (consp count)
        (append type (list :count (car count)))
        type)))

(defvar *vst3-grovel-types* (make-hash-table :test 'equal))

(with-open-file (out (asdf:system-relative-pathname :dgw "src/XXXvst3-grovel.lisp") :direction :output :if-exists :supersede)
  (emit '(in-package :vst3) out)
  (terpri out)
  (terpri out)
  (emit `(include ,*vst3-c-api-h*) out)
  (terpri out)
  (loop for typedef in *h*
        if (string-equal "typedef" (car typedef))
          do (let ((sym (extract-name typedef)))
               (setf (gethash (name-c sym) *vst3-grovel-types*)
                     `(,(cadr typedef)
                       ,@(let ((last (car (last typedef))))
                           (when (consp last)
                             `(:count ,(car last))))))))
  (let* ((struct
           (loop for i in *h*
                   thereis (and (consp (cddr i))
                                (string-equal "Steinberg_PClassInfoW"
                                              (cadr i))
                                i)))
         (slots (loop for i in (caddr struct)
                      for name = (extract-name i)
                      for name-c = (name-c name)
                      for name-lisp = (name-lisp name)
                      for type = (grovel-type i)
                      collect `(,name-lisp ,name-c :type ,@type)))
         (form `(cstruct-and-class-item ,(name-lisp (cadr struct)) ,(format nil "struct ~a" (string (cadr struct)))
                                        ,@slots)))
    (terpri out)
    (pprint-logical-block (out form :prefix "(" :suffix ")")
      (emit (pprint-pop) out)
      (write-char #\space out)
      (emit (pprint-pop) out)
      (write-char #\space out)
      (emit (pprint-pop) out)
      (pprint-indent :block 1 out)
      (loop (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory out)
            (emit (pprint-pop) out)))))
;;→ 
;;   (READ-VST3-C-API-H::|struct| READ-VST3-C-API-H::|Steinberg_PClassInfoW|
;;    ((READ-VST3-C-API-H::|Steinberg_TUID| READ-VST3-C-API-H::|cid|)
;;     (READ-VST3-C-API-H::|Steinberg_int32| READ-VST3-C-API-H::|cardinality|)
;;     (READ-VST3-C-API-H::|Steinberg_char8| READ-VST3-C-API-H::|category| (32))
;;     (READ-VST3-C-API-H::|Steinberg_char16| READ-VST3-C-API-H::|name| (64))
;;     (READ-VST3-C-API-H::|Steinberg_uint32| READ-VST3-C-API-H::|classFlags|)
;;     (READ-VST3-C-API-H::|Steinberg_char8| READ-VST3-C-API-H::|subCategories|
;;      (128))
;;     (READ-VST3-C-API-H::|Steinberg_char16| READ-VST3-C-API-H::|vendor| (64))
;;     (READ-VST3-C-API-H::|Steinberg_char16| READ-VST3-C-API-H::|version| (64))
;;     (READ-VST3-C-API-H::|Steinberg_char16| READ-VST3-C-API-H::|sdkVersion| (64)))) 
;;⇒ NIL

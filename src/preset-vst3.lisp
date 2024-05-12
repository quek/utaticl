(in-package :dgw)

#|
VST_SDK/vst3sdk/public.sdk/source/vst/vstpresetfile.h

   VST 3 Preset File Format Definition
   ===================================

0   +---------------------------+
    | HEADER                    |
    | header id ('VST3')        |       4 Bytes
    | version                   |       4 Bytes (int32)
    | ASCII-encoded class id    |       32 Bytes
 +--| offset to chunk list      |       8 Bytes (int64)
 |  +---------------------------+
 |  | DATA AREA                 |<-+
 |  | data of chunks 1..n       |  |
 |  ...                       ...  |
 |  |                           |  |
 +->+---------------------------+  |
    | CHUNK LIST                |  |
    | list id ('List')          |  |    4 Bytes
    | entry count               |  |    4 Bytes (int32)
    +---------------------------+  |
    |  1..n                     |  |
    |  +----------------------+ |  |
    |  | chunk id             | |  |    4 Bytes
    |  | offset to chunk data |----+    8 Bytes (int64)
    |  | size of chunk data   | |       8 Bytes (int64)
    |  +----------------------+ |
EOF +---------------------------+

|#

(defconstant +preset-vst3-version+ 1)
(defconstant +preset-vst3-cid-pos+ 8)
(defconstant +preset-vst3-list-offset-pos+ 40)

(defmethod cid ((self preset-vst3))
  (let ((buffer (.buffer self))
        (cid (make-array 32 :element-type '(unsigned-byte 8))))
    (setf (vst3-impl::.cursor buffer) +preset-vst3-cid-pos+)
    (loop with n = (parse-integer (vst3-impl::read-string buffer 32) :radix 16)
          for i below 32
          do (setf (aref cid i)
                   (ldb (byte 8 i) n)))
    cid))

(defmethod preset-vst3-from-base64 (base64)
  (let* ((vec (qbase64:decode-string base64))
         (bstream (make-instance 'vst3-impl::bstream :buffer vec)))
    (make-instance 'preset-vst3 :buffer bstream)))

(defmethod preset-vst3-to-base64 ((self preset-vst3))
  (let ((vec (vst3-impl::.buffer (.buffer self))))
    (qbase64:encode-bytes (subseq vec 0
                                  (vst3-impl::.tail (.buffer self))))))

(defmethod preset-save ((self preset-vst3) module)
  (let ((buffer (.buffer self))
        (chunks nil))
    (setf (vst3-impl::.cursor buffer) 0)
    ;; HEAD
    (vst3-impl::write-string$ buffer "VST3")
    (vst3-impl::write-integer buffer +preset-vst3-version+ 4)
    (vst3-impl::write-string$ buffer
                              (with-output-to-string (out)
                                (loop for i across (.id module)
                                      do (format out "~2,'0X" i))))
    (vst3-impl::write-integer buffer 0 8)
    ;; Component State
    (push (list "Comp" (vst3-impl::.cursor buffer)
                (vst3-ffi::get-state (.component module) (vst3-impl::ptr buffer))
                (vst3-impl::.cursor buffer))
          chunks)
    ;; Controller State
    (push (list "Cont" (vst3-impl::.cursor buffer)
                (vst3-ffi::get-state (.controller module) (vst3-impl::ptr buffer))
                (vst3-impl::.cursor buffer))
          chunks)
    ;; offset to chunk list
    (let ((pos (vst3-impl::.cursor buffer)))
      (setf (vst3-impl::.cursor buffer) +preset-vst3-list-offset-pos+)
      (vst3-impl::write-integer buffer pos 8)
      (setf (vst3-impl::.cursor buffer) pos))
    (vst3-impl::write-string$ buffer "List")
    (vst3-impl::write-integer buffer (length chunks) 4)
    (loop for (id offset size) in chunks
          do (vst3-impl::write-string$ buffer id)
             (vst3-impl::write-integer buffer offset 8)
             (vst3-impl::write-integer buffer size 8))))

(defmethod preset-load ((self preset-vst3) module)
  (let ((buffer (.buffer self))
        (pos 0)
        (chunks nil))
    (setf (vst3-impl::.cursor buffer) +preset-vst3-list-offset-pos+)
    (setf pos (vst3-impl::read-integer buffer 8))
    (setf (vst3-impl::.cursor buffer) (+ pos 4))
    (loop repeat (vst3-impl::read-integer buffer 4)
          do (push (list (vst3-impl::read-string buffer 4)
                         (vst3-impl::read-integer buffer 8)
                         (vst3-impl::read-integer buffer 8))
                   chunks))
    (let ((chunk (find "Comp" chunks :key #'car :test #'string=)))
      (when chunk
        (setf (vst3-impl::.cursor buffer) (cadr chunk))
        (setf (vst3-impl::.tail buffer) (+ (cadr chunk) (caddr chunk)))
        (vst3-ffi::set-state (.component module) buffer)
        (setf (vst3-impl::.cursor buffer) (cadr chunk))
        (setf (vst3-impl::.tail buffer) (+ (cadr chunk) (caddr chunk)))
        (vst3-ffi::set-component-state (.controller module) buffer)))
    (let ((chunk (find "Cont" chunks :key #'car :test #'string=)))
      (when chunk
        (setf (vst3-impl::.cursor buffer) (cadr chunk))
        (setf (vst3-impl::.tail buffer) (+ (cadr chunk) (caddr chunk)))
        (vst3-ffi::set-state (.controller module) buffer)))))

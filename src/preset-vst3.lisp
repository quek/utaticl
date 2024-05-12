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
(defconstant +preset-vst3-list-offset-pos+ 40)

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
      (vst3-impl::write-integer buffer pos 64)
      (setf (vst3-impl::.cursor buffer) pos))
    (vst3-impl::write-string$ buffer "List")
    (vst3-impl::write-integer buffer (length chunks) 4)
    (loop for (id offset size) in chunks
          do (vst3-impl::write-string$ buffer id)
             (vst3-impl::write-integer buffer offset 8)
             (vst3-impl::write-integer buffer size 8))))

#+nil
(let ((x (make-instance 'preset-vst3 )))
  (preset-save x (module-vst3-load "c:/Program Files/Common Files/VST3/Dexed.vst3"))
  (vst3-impl::.buffer (.buffer x)))

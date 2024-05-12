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

(defun preset-write-string (string buffer)
  (loop for c across (sb-ext:string-to-octets string :external-format :utf-8)
        do (vector-push-extend c buffer)))

(defun preset-write-integer (integer size buffer)
  (loop for i below size
        do (vector-push-extend (ldb (byte 8 i) integer) buffer)))

(defmethod preset-save ((self preset-vst3) module)
  (let ((buffer (.buffer self)))
    (setf (fill-pointer buffer) 0)
    ;; HEAD
    (preset-write-string "VST3" buffer)
    (preset-write-integer +preset-vst3-version+ 4 buffer)
    (preset-write-string (with-output-to-string (out)
                           (loop for i across (.id module)
                                 do (format out "~2,'0X" i)))
                         buffer)
    (preset-write-integer 0 8 buffer)
    ;; Component State
    
))

#+nil
(let ((x (make-instance 'preset-vst3 )))
  (preset-save x (module-vst3-load "c:/Program Files/Common Files/VST3/Dexed.vst3"))
  (.buffer x))
;;⇒ #(86 83 84 51 1 0 0 0 48 49 69 70 67 68 65 66 56 50 57 49 69 66 70 65 52 52 52
;;     55 53 51 52 50 52 52 54 53 55 56 54 52 0 0 0 0 0 0 0 0)

(in-package :vst3)

(defun int32-to-bytes (int32)
  "32ビット整数をバイト配列に変換します。"
  (let ((bytes (make-array 4 :element-type 'unsigned-byte :initial-element 0)))
    (setf (aref bytes 0) (ldb (byte 8 24) int32))
    (setf (aref bytes 1) (ldb (byte 8 16) int32))
    (setf (aref bytes 2) (ldb (byte 8 8) int32))
    (setf (aref bytes 3) (ldb (byte 8 0) int32))
    bytes))

(defun make-tuid (&rest args)
  "32ビット整数のリストから Steinberg_TUID を生成します。"
  (let ((tuid (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for i from 0 below (length args)
          do (let ((bytes (int32-to-bytes (nth i args))))
               (loop for j from 0 below 4
                     do (setf (aref tuid (+ (* i 4) j)) (aref bytes j)))))
    tuid))

(defvar *funknown-iid*
  (make-tuid #x00000000 #x00000000 #xC0000000 #x00000046))

(defvar *iplugin-factory2-iid* (make-tuid #x0007B650 #xF24B4C0B #xA464EDB9 #xF00B2ABB))
(defvar *iplugin-factory3-iid* (make-tuid #x4555A2AB #xC1234E57 #x9B122910 #x36878931))



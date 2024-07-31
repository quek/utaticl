(in-package :src-ffi)

(cffi:define-foreign-library samplerate
  (:windows "samplerate.dll"))

(cffi:use-foreign-library samplerate)


(cffi:defcstruct src-data
  (data-in (:pointer :float))
  (data-out (:pointer :float))
  (input-frames :long)
  (output-frames :long)
  (input-frames-used :long)
  (output-frames-gen :long)
  (end-of-input :int)
  (src-ratio :double))

(cffi:defcfun ("src_strerror" src-strerror) :string
  (error :int))

(cffi:defcfun ("src_simple" src-simple) :int
  (data (:pointer (:struct src-data)))
  (converter-type :int)
  (channels :int))

(defconstant +src-sinc-best-quality+ 0)
(defconstant +src-sinc-medium-quality+ 1)
(defconstant +src-sinc-fastest+ 2)
(defconstant +src-zero-order-hold+ 3)
(defconstant +src-linear+ 4)

(defun simple (in ratio-out/in nchannes)
  (let* ((nframes-in (/ (length in) nchannes))
         (nframes-out (ceiling (* nframes-in ratio-out/in)))
         (out (make-array (* nframes-out nchannes)
                          :element-type 'single-float
                          :initial-element .0)))
    (sb-sys:with-pinned-objects (in out)
      (cffi:with-foreign-objects ((data '(:struct src-data)))
        (cffi:with-foreign-slots ((data-in data-out
                                           input-frames output-frames
                                           input-frames-used output-frames-gen
                                           src-ratio)
                                  data (:struct src-data))
          (setf data-in (sb-sys:vector-sap in))
          (setf data-out (sb-sys:vector-sap out))
          (setf input-frames nframes-in)
          (setf output-frames nframes-out)
          (setf input-frames-used 0)
          (setf output-frames-gen 0)
          (setf src-ratio ratio-out/in)
          (let ((ret (src-simple data +src-sinc-best-quality+ nchannes)))
            (if (zerop ret)
                out
                (error (src-strerror ret)))))))))

#+nil
(let ((audio (make-instance 'dgw::seq-audio :path "d:/Samples/AU5 Sample/Kick/Au5-Kick Tight 1.wav")))
  (simple (dgw::.data audio) 44100.0d0 48000.0d0 (dgw::.nchannels audio)))

#+nil
(src-strerror 16)




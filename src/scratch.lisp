(in-package :dgw)

(let* ((clip1 (make-instance 'clip-note))
       (clip2 (copy clip1)))
  (list (.seq clip1) (.seq clip2))
  (mapc #'describe
   (with-serialize-context ()
     (deserialize
      (with-serialize-context ()
        (serialize (list clip1 clip2)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
  (let ((module (module-vst3-load
                 "c:/Program Files/Common Files/VST3/Dexed.vst3"
                 ;; "c:/Program Files/Common Files/VST3/DS Thorn.vst3"
                 ;; "c:/Program Files/Common Files/VST3/MeldaProduction/MSoundFactory.vst3"
                 ;; "c:/Program Files/Common Files/VST3/Vital.vst3"
                 ))
        (buffer (make-instance 'vst3-impl::bstream)))
    (unwind-protect
         (progn
           (vst3-impl::add-ref buffer)
           (initialize module)
           (start module)

           (let ((result (vst3-ffi::get-state (.component module) (vst3-impl::ptr buffer))))
             (assert (= result sb:+k-result-ok+)))

           (setf (vst3-impl::.cursor buffer) 0)
           (let ((result (vst3-ffi::set-state (.component module) (vst3-impl::ptr buffer))))
             (assert (= result sb:+k-result-ok+))))

      (stop module)
      (terminate module))
    module))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(pa:initialize)

(pa:print-devices)
(let* ((i 29)
       (device-info (pa:get-device-info i))
       (output-parameters (pa:make-stream-parameters)))
  (describe device-info)
  (describe (pa:get-host-api-info (pa:device-info-host-api device-info)))
  (setf (pa:stream-parameters-device output-parameters) i)
  (setf (pa:stream-parameters-channel-count output-parameters)
         (pa:device-info-max-output-channels device-info))
  (setf (pa:stream-parameters-sample-format output-parameters) :float)
  (setf (pa:stream-parameters-suggested-latency output-parameters)
        (pa:device-info-default-low-output-latency device-info))
  (let ((stream (pa:open-stream nil
                                output-parameters
                                48000.0d0
                                1024
                                0)))
    (unwind-protect
         (progn
           ;;(describe stream)
           (describe (pa:get-stream-info stream))
           )
      (pa:close-stream stream))))


(cffi:with-foreign-objects ((min-size :long)
                            (max-size :long)
                            (preferred-size :long)
                            (granularity :long))
  ;; https://files.portaudio.com/docs/v19-doxydocs/pa__asio_8h.html
  (cffi:foreign-funcall "PaAsio_GetAvailableBufferSizes"
                        :int 29
                        :pointer min-size
                        :pointer max-size
                        :pointer preferred-size
                        :pointer granularity
                        :int)
  (values (cffi:mem-ref min-size :long)
          (cffi:mem-ref max-size :long)
          (cffi:mem-ref preferred-size :long)
          (cffi:mem-ref granularity :long)))
;;⇒ 1024
;;   1024
;;   1024
;;   1



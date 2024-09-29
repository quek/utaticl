(in-package :utaticl.core)

(let ((n 0))
 (defun foo ()
   (print (list (incf n) (.steady-time *app*) (.play-start (car (.projects *app*)))))))

(sb-thread:make-thread
 (lambda ()
   (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
     (utaticl::with-ole
       (with-thraed-pool
         (setf *app* (make-instance 'app :backend :sdl-vulkan))
         (unwind-protect
              (progn
                (audio-thread-start *app*)
                (setf (.play-p (car (.projects *app*))) t)
                (loop do (sleep 1) (foo)))
           (utaticl.core:terminate utaticl.core:*app*)))))
   )
 :name "UTATICL")
;;⇒ #<SB-THREAD:THREAD tid=23588 "UTATICL" RUNNING {10704A1003}>


;;; ok 落ちない
(cffi:with-foreign-object (buffer :float 1024)
  (loop while (.play-p (car (.projects *app*)))
        do (audio-loop buffer)
           (sleep .01)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((module (cadr (.modules (.master-track (car (.projects *app*))))))
       (process (.clap-process module))
       (audio-inputs (clap::make-clap-audio-buffer :ptr (clap:clap-process.audio-inputs process)))
       (audio-outputs (clap::make-clap-audio-buffer :ptr (clap:clap-process.audio-outputs process)))
       (audio-outputs2 (clap::make-clap-audio-buffer
                        :ptr (cffi:inc-pointer (clap:clap-process.audio-outputs process)
                                               (autowrap:sizeof 'clap:clap-audio-buffer-t))))
       (audio-outputs3 (clap::make-clap-audio-buffer
                        :ptr (cffi:inc-pointer (clap:clap-process.audio-outputs process)
                                               (* 2 (autowrap:sizeof 'clap:clap-audio-buffer-t)))))
       (ao (utaticl.clap::process-audio-outputs process)))
  (values
   (clap:clap-process.audio-inputs-count process)
   (clap:clap-process.audio-outputs-count process)
   (clap:clap-process.audio-outputs*.data32  process)
   (cffi:mem-aref (clap:clap-process.audio-outputs*.data32 process) :pointer 0)
   (cffi:mem-aref (clap:clap-audio-buffer.data32 audio-outputs) :pointer 0)
   (cffi:mem-aref (clap:clap-audio-buffer.data32 audio-outputs2) :pointer 0)
   (cffi:mem-aref (clap:clap-audio-buffer.data32 audio-outputs3) :pointer 0)

   (mapcar #'.buffer (.inputs (.process-data (.track module))))
   (mapcar #'.buffer (.outputs (.process-data (.track module))))
   (.buffer (car (.outputs (.process-data (.track module)))))))
;;⇒ 1
;;   3
;;   #.(SB-SYS:INT-SAP #X1E2B7120)
;;   #.(SB-SYS:INT-SAP #X1E2B8260)
;;   #.(SB-SYS:INT-SAP #X1E2B8260)
;;   #.(SB-SYS:INT-SAP #X1E2B9270)
;;   #.(SB-SYS:INT-SAP #X1E2BA280)
;;   (#.(SB-SYS:INT-SAP #X1E2B8260) #.(SB-SYS:INT-SAP #X1E2B9270)
;;    #.(SB-SYS:INT-SAP #X1E2BA280))
;;   (#.(SB-SYS:INT-SAP #X1E2BB290) #.(SB-SYS:INT-SAP #X1E2BC2A0)
;;    #.(SB-SYS:INT-SAP #X1E2BD2B0))
;;   #.(SB-SYS:INT-SAP #X1E2BB290)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(wav:read-wav-file "D:\\Samples\\Audiolatry - Anime Vocals\\Vocal_Shots\\Audiolatry_AV_Vocal_One_Shot_Female_Yuck.wav")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



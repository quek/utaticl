(in-package :utaticl.core)

(setf *config* (make-instance 'config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cffi:load-foreign-library "Winmm.dll")
;;⇒ #<CFFI:FOREIGN-LIBRARY WINMM.DLL-483 "Winmm.dll">

(cffi:defcfun "midiInGetNumDevs" :unsigned-int)
;;⇒ MIDIINGETNUMDEVS
(midiInGetNumDevs)
;;⇒ 1

(cffi:defcfun "midiOutGetNumDevs" :unsigned-int)
;;⇒ MIDIOUTGETNUMDEVS
(midiOutGetNumDevs)
;;⇒ 2

(defconstant MAXPNAMELEN 32)

(cffi:defcstruct MIDIINCAPS
  (wMid :uint16)
  (wPid :uint16)
  (vDriverVersion :unsigned-int)
  (szPname :uint16 :count #.MAXPNAMELEN)
  (dwSupport :unsigned-int))
;;⇒ (:STRUCT MIDIINCAPS)

(cffi:defcstruct MIDIOUTCAPS
  (wMid :uint16)
  (wPid :uint16)
  (vDriverVersion :unsigned-int)
  (szPname :uint16 :count #.MAXPNAMELEN)
  (wTechnology :uint16)
  (wVoices :uint16)
  (wNotes :uint16)
  (wChannelMask :uint16)
  (dwSupport :unsigned-int))
;;⇒ (:STRUCT MIDIOUTCAPS)

(cffi:defcfun "midiInGetDevCapsW" :unsigned-int
  (uDeviceID :unsigned-int)
  (pmic (:pointer (:struct MIDIINCAPS)))
  (cbmic :unsigned-int))
;;⇒ MIDIINGETDEVCAPS

(cffi:defcfun "midiOutGetDevCapsW" :unsigned-int
  (uDeviceID :unsigned-int)
  (pmoc (:pointer (:struct MIDIOUTCAPS)))
  (cbmoc :unsigned-int))
;;⇒ MIDIOUTGETDEVCAPS

(cffi:with-foreign-object (pmic '(:struct MIDIINCAPS))
  (loop for device-id below (midiInGetNumDevs)
        collect (progn
                  (midiInGetDevCapsW 0 pmic (cffi:foreign-type-size '(:struct MIDIINCAPS)))
                  (cffi:foreign-string-to-lisp
                   (cffi:foreign-slot-value pmic '(:struct MIDIINCAPS) 'szPname)
                   :encoding :utf-16le))))
;;⇒ ("SP-404MKII-G")

(cffi:with-foreign-object (pmic '(:struct MIDIOUTCAPS))
  (loop for device-id below (midiOutGetNumDevs)
        collect (progn
                  (midiOutGetDevCapsW 0 pmic (cffi:foreign-type-size '(:struct MIDIOUTCAPS)))
                  (cffi:foreign-string-to-lisp
                   (cffi:foreign-slot-value pmic '(:struct MIDIOUTCAPS) 'szPname)
                   :encoding :utf-16le))))
;;⇒ ("Microsoft GS Wavetable Synth" "Microsoft GS Wavetable Synth")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((*project* (make-instance 'project)))
 (with-open-file (in "c:/Users/ancient/quicklisp/local-projects/utaticl/user/project/20241015.lisp"
                     :direction :input)
   (let ((project (with-serialize-context () (deserialize (read in)))))
     project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((clip
        (make-instance 'clip-audio :path "D:\\Samples\\Audiolatry - Anime Vocals\\Vocal_Shots\\Audiolatry_AV_Vocal_One_Shot_Female_Yuck.wav")))
  (copy clip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let* ((process-data (.process-data (.master-track (car (.projects *app*)))))
       (module (cadr (.modules (.master-track (car (.projects *app*))))))
       (pd (.process-data module))
       (p (.wrap pd))
       (in (sb::make-vst-audio-bus-buffers :ptr (sb:vst-process-data.inputs p)))
       (out (sb::make-vst-audio-bus-buffers :ptr (sb:vst-process-data.outputs p))))
  (values (sb:vst-audio-bus-buffers.num-channels in)
          (sb:vst-audio-bus-buffers.silence-flags in)
          (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32* in)
          (buffer-at (car (.inputs process-data)) 0)
          (cffi:mem-aref (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32 in) :pointer 0)
          (cffi:mem-aref (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32 in) :pointer 1)
          (buffer-at (car (.inputs process-data)) 1)
          "--"
          (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32* out)
          (buffer-at (car (.outputs process-data)) 0)
          (cffi:mem-aref (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32 out) :pointer 0)
          (cffi:mem-aref (sb:vst-audio-bus-buffers.vst-audio-bus-buffers-channel-buffers32 out) :pointer 1)
          (buffer-at (car (.outputs process-data)) 1)))
;;⇒ 2
;;   0
;;   #.(SB-SYS:INT-SAP #X1E30B4F0)
;;   #.(SB-SYS:INT-SAP #X02ABE260)
;;   #.(SB-SYS:INT-SAP #X1E30B4F0)
;;   #.(SB-SYS:INT-SAP #X1E30BCF0)
;;   #.(SB-SYS:INT-SAP #X02ABEA60)
;;   "--"
;;   #.(SB-SYS:INT-SAP #X02ABE260)
;;   #.(SB-SYS:INT-SAP #X1E30B4F0)
;;   #.(SB-SYS:INT-SAP #X02ABE260)
;;   #.(SB-SYS:INT-SAP #X02ABEA60)
;;   #.(SB-SYS:INT-SAP #X1E30BCF0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



(in-package :utaticl.core)

(cffi:defcfun "midiInGetNumDevs" :unsigned-int)
(cffi:defcfun "midiOutGetNumDevs" :unsigned-int)

(defconstant MAXPNAMELEN 32)

(cffi:defcstruct MIDIINCAPS
  (wMid :uint16)
  (wPid :uint16)
  (vDriverVersion :unsigned-int)
  (szPname :uint16 :count #.MAXPNAMELEN)
  (dwSupport :unsigned-int))

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

(cffi:defcfun "midiInGetDevCapsW" :unsigned-int
  (uDeviceID :unsigned-int)
  (pmic (:pointer (:struct MIDIINCAPS)))
  (cbmic :unsigned-int))

(cffi:defcfun "midiOutGetDevCapsW" :unsigned-int
  (uDeviceID :unsigned-int)
  (pmoc (:pointer (:struct MIDIOUTCAPS)))
  (cbmoc :unsigned-int))

(defun midi-devices-in ()
  (cffi:with-foreign-object (pmic '(:struct MIDIINCAPS))
    (loop for device-id below (midiInGetNumDevs)
          collect (progn
                    (midiInGetDevCapsW 0 pmic (cffi:foreign-type-size '(:struct MIDIINCAPS)))
                    (cffi:foreign-string-to-lisp
                     (cffi:foreign-slot-value pmic '(:struct MIDIINCAPS) 'szPname)
                     :encoding :utf-16le)))))

(defun midi-devices-out ()
  (cffi:with-foreign-object (pmic '(:struct MIDIOUTCAPS))
    (loop for device-id below (midiOutGetNumDevs)
          collect (progn
                    (midiOutGetDevCapsW 0 pmic (cffi:foreign-type-size '(:struct MIDIOUTCAPS)))
                    (cffi:foreign-string-to-lisp
                     (cffi:foreign-slot-value pmic '(:struct MIDIOUTCAPS) 'szPname)
                     :encoding :utf-16le)))))

(defconstant CALLBACK_FUNCTION #x00030000)

(cffi:defcfun "midiInOpen" :unsigned-int
  (phmi :pointer)
  (uDeviceID :unsigned-int)
  (dwCallback :pointer)
  (dwInstance :pointer)
  (fdwOpen :unsigned-int))

(cffi:defcfun "midiInClose" :unsigned-int
  (hmi :pointer))

(cffi:defcallback MidiInProc :void
    ((hMidiIn :pointer)
     (wMsg :unsigned-int)
     (dwInstance :pointer)
     (dwParam1 :pointer)
     (dwParam2 :pointer))
  (midi-in-proc hMidiIn wMsg dwInstance dwParam1 dwParam2))

(defun midi-in-proc (hMidiIn wMsg dwInstance dwParam1 dwParam2)
  (print (list hMidiIn wMsg dwInstance dwParam1 dwParam2)))

(cffi:defcfun "midiInStart" :unsigned-int
  (hmi :pointer))

(cffi:defcfun "midiInStop" :unsigned-int
  (hmi :pointer))

(defmacro with-midi-in-callback ((uDeviceID) &body body)
  (let ((hmi (gensym "HMI")))
    `(cffi:with-foreign-object (phmi :pointer)
       (midiInOpen phmi ,uDeviceID (cffi:callback MidiInProc) (cffi:null-pointer)
                   CALLBACK_FUNCTION)
       (let ((,hmi (cffi:mem-ref phmi :pointer)))
         (unwind-protect
              (progn
                (midiInStart ,hmi)
                (unwind-protect
                     (progn ,@body)
                  (midiInStop ,hmi)))
           (midiInClose ,hmi))))))

#+nil
(with-midi-in-callback (0)
  (sleep 3))
;;→ 
;;   (#.(SB-SYS:INT-SAP #X0077A7F0) 961 #.(SB-SYS:INT-SAP #X00000000)
;;    #.(SB-SYS:INT-SAP #X00000000) #.(SB-SYS:INT-SAP #X00000000)) 
;;   (#.(SB-SYS:INT-SAP #X0077A7F0) 962 #.(SB-SYS:INT-SAP #X00000000)
;;    #.(SB-SYS:INT-SAP #X00000000) #.(SB-SYS:INT-SAP #X00000000)) 
;;⇒ NIL



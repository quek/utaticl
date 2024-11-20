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
  (dwInstance :unsigned-int)
  (fdwOpen :unsigned-int))

(cffi:defcfun "midiInClose" :unsigned-int
  (hmi :pointer))

(cffi:defcallback MidiInProc :void
    ((hMidiIn :pointer)
     (wMsg :unsigned-int)
     (dwInstance :unsigned-int)
     (dwParam1 :unsigned-int)
     (dwParam2 :unsigned-int))
  (midi-in-proc hMidiIn wMsg dwInstance dwParam1 dwParam2))

(defconstant MIM_OPEN         #x3C1)
(defconstant MIM_CLOSE        #x3C2)
(defconstant MIM_DATA         #x3C3)
(defconstant MIM_LONGDATA     #x3C4)
(defconstant MIM_ERROR        #x3C5)
(defconstant MIM_LONGERROR    #x3C6)
(defconstant MIM_MOREDATA     #x3CC)

(defvar *midi-in-callback* (make-hash-table))

(defun midi-in-proc (hMidiIn wMsg dwInstance dwParam1 dwParam2)
  (declare (ignore hMidiIn dwParam2))
  (ecase wMsg
    (#.MIM_OPEN)
    (#.MIM_CLOSE)
    (#.MIM_DATA
     (let* ((event (ldb (byte 4 4) dwParam1))
            (event (case event
                     (9 :on)
                     (8 :off)
                     (t event)))
            (channel (ldb (byte 4 0) dwParam1))
            (key (ldb (byte 8 8) dwParam1))
            (velocity (ldb (byte 8 16) dwParam1)))
       (funcall (gethash dwInstance *midi-in-callback*)
                event channel key velocity)))
    (#.MIM_LONGDATA)
    (#.MIM_ERROR)
    (#.MIM_LONGERROR)
    (#.MIM_MOREDATA)))

(cffi:defcfun "midiInStart" :unsigned-int
  (hmi :pointer))

(cffi:defcfun "midiInStop" :unsigned-int
  (hmi :pointer))

(defmacro with-midi-in-callback ((uDeviceID event channel key velocity) &body body)
  (let ((hmi (gensym "HMI")))
    `(cffi:with-foreign-object (phmi :pointer)
       (setf (gethash ,uDeviceID *midi-in-callback*)
             (lambda (,event ,channel ,key ,velocity)
               ,@body))
       (midiInOpen phmi ,uDeviceID (cffi:callback MidiInProc) ,uDeviceID
                   CALLBACK_FUNCTION)
       (let ((,hmi (cffi:mem-ref phmi :pointer)))
         (unwind-protect
              (progn
                (midiInStart ,hmi)
                (unwind-protect
                     (sleep 3)
                  (midiInStop ,hmi)))
           (midiInClose ,hmi))))))

#+nil
(with-midi-in-callback (0 event channel key velocity)
  (print (list "callback" event channel key velocity)))



(in-package :utaticl.core)

(defmethod initialize-instance :after ((self process-data-vst3)
                                       &key (audio-input-bus-count 1)
                                         (audio-output-bus-count 1))
  (let ((wrap (autowrap:alloc '(:struct (sb:vst-process-data))))
        (inputs (make-instance 'audio-bus-buffers :nbuses audio-input-bus-count))
        (outputs (make-instance 'audio-bus-buffers :nbuses audio-output-bus-count))
        (input-events (make-instance 'vst3-impl::event-list))
        (output-events (make-instance 'vst3-impl::event-list))
        (context (autowrap:calloc '(:struct (sb:vst-process-context)))))
    (setf (.wrap self) wrap)
    (setf (sb:vst-process-data.process-mode wrap)
          sb:+vst-process-modes-k-realtime+)
    (setf (sb:vst-process-data.symbolic-sample-size wrap)
          sb:+vst-symbolic-sample-sizes-k-sample32+)
    (setf (sb:vst-process-data.num-samples wrap)
          (.frames-per-buffer *config*))

    (setf (.inputs self) inputs)
    (setf (sb:vst-process-data.num-inputs wrap) audio-input-bus-count)
    (setf (sb:vst-process-data.inputs wrap) (.ptr inputs))
    (setf (.outputs self) outputs)
    (setf (sb:vst-process-data.num-outputs wrap) audio-output-bus-count)
    (setf (sb:vst-process-data.outputs wrap) (.ptr outputs))

    (setf (.input-events self) input-events)
    (setf (sb:vst-process-data.input-events wrap) (vst3-impl::ptr input-events))
    (setf (.output-events self) output-events)
    (setf (sb:vst-process-data.output-events wrap) (vst3-impl::ptr output-events))

    (setf (.context self) context)
    (setf (sb:vst-process-data.process-context wrap) (autowrap:ptr context))
    (setf (sb:vst-process-context.state context) 0)
    (setf (sb:vst-process-context.sample-rate context) (.sample-rate *config*))
    (setf (sb:vst-process-context.time-sig-numerator context) 4)
    (setf (sb:vst-process-context.time-sig-denominator context) 4)

    (sb-ext:finalize self (lambda ()
                            (log:trace "process-data finalize free" wrap)
                            (autowrap:free context)
                            (autowrap:free wrap)
                            (vst3-impl::release input-events)
                            (vst3-impl::release output-events)))))

(defmethod apply-from ((self process-data-vst3) (process-data process-data) &key module)
  (let* ((context (.context self))
         (project (.project module))
         (bpm (.bpm project))
         (play-time (.play-start *project*)))
    (setf (sb:vst-process-context.state context)
          (logand (if (.play-p project) sb:+vst-process-context-states-and-flags-k-playing+ 0)
                  sb:+vst-process-context-states-and-flags-k-tempo-valid+
                  sb:+vst-process-context-states-and-flags-k-project-time-music-valid+
                  sb:+vst-process-context-states-and-flags-k-bar-position-valid+))
    (setf (sb:vst-process-context.tempo context) (coerce bpm 'double-float))
    (setf (sb:vst-process-context.project-time-music context) play-time)
    (setf (sb:vst-process-context.bar-position-music context)
          (coerce (floor (/ play-time 4)) 'double-float))
    (setf (sb:vst-process-context.project-time-samples context)
          ;; TODO これあってる？
          (floor (* (/ play-time (/ bpm 60.0)) (.sample-rate *config*)))))

  (setup-audio-buffer (.inputs self)
                      (.inputs process-data))
  (setup-audio-buffer (.outputs self)
                      (.outputs process-data))

  (setf (sb:vst-process-data.input-parameter-changes (.wrap self))
        (vst3-impl::ptr (.parameter-changes-out module)))
  (setf (sb:vst-process-data.output-parameter-changes (.wrap self))
        (vst3-impl::ptr (.parameter-changes-in module)))

  (note-buffer->vst3 self)

  (prepare (.parameter-changes-in module))
  (prepare (.parameter-changes-out module))

  (loop for message = (sb-concurrency:receive-message-no-hang
                       (.param-changes-mbox-in module))
        with changes = (.parameter-changes-out module)
        while message
        do (destructuring-bind (id value sample-offset) message
             (let ((queue (or (loop for i below (vst3-impl::get-parameter-count changes)
                                    for queue = (vst3-impl::get-parameter-data-wrap changes i)
                                      thereis (and (= (vst3-impl::get-parameter-id queue)
                                                      id)
                                                   queue))
                              (cffi:with-foreign-object (id-ptr :uint)
                                (setf (cffi:mem-ref id-ptr :uint) id)
                                (vst3-impl::add-parameter-data-wrap
                                 changes id-ptr (cffi:null-pointer))))))
               (vst3-impl::add-point queue sample-offset value (cffi:null-pointer))))))

(defmethod note-buffer->vst3 ((self process-data-vst3))
  (prepare (.input-events self))
  (prepare (.output-events self))
  (loop with note-buffer = (.input-events *process-data*)
        with process-data = self
        for event across (.events note-buffer)
        for note across (.notes note-buffer)
        for sample-offset across (.sample-offsets note-buffer)
        do (case event
             (:on
              (autowrap:with-alloc (event '(:struct (sb:vst-event)))
                (setf (sb:vst-event.bus-index event) 0) ;TODO
                (setf (sb:vst-event.sample-offset event) sample-offset)
                (setf (sb:vst-event.ppq-position event) .0d0) ;TODO
                (setf (sb:vst-event.flags event) sb:+vst-event-event-flags-k-is-live+) ;TODO
                (setf (sb:vst-event.type event) sb:+vst-event-event-types-k-note-on-event+)
                (setf (sb:vst-event.vst-event-note-on.channel event) (.channel note))
                (setf (sb:vst-event.vst-event-note-on.pitch event) (.key note))
                (setf (sb:vst-event.vst-event-note-on.tuning event) .0)
                (setf (sb:vst-event.vst-event-note-on.velocity event)
                      (coerce (.velocity note) 'single-float))
                (setf (sb:vst-event.vst-event-note-on.note-id event) -1)
                (setf (sb:vst-event.vst-event-note-on.length event) 0)
                (vst3-impl::add-event (.input-events process-data)
                                      (autowrap:ptr event))))
             (:off
              (autowrap:with-alloc (event '(:struct (sb:vst-event)))
                (setf (sb:vst-event.bus-index event) 0) ;TODO
                (setf (sb:vst-event.sample-offset event) sample-offset)
                (setf (sb:vst-event.ppq-position event) .0d0) ;TODO
                (setf (sb:vst-event.flags event) sb:+vst-event-event-flags-k-is-live+) ;TODO
                (setf (sb:vst-event.type event) sb:+vst-event-event-types-k-note-off-event+)
                (setf (sb:vst-event.vst-event-note-off.channel event) (.channel note))
                (setf (sb:vst-event.vst-event-note-off.pitch event) (.key note))
                (setf (sb:vst-event.vst-event-note-off.tuning event) .0)
                (setf (sb:vst-event.vst-event-note-off.velocity event) 1.0)
                (setf (sb:vst-event.vst-event-note-off.note-id event) -1)
                (vst3-impl::add-event (.input-events process-data)
                                      (autowrap:ptr event)))))))


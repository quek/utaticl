(in-package :utaticl.core)

(defmethod initialize-instance :after ((self module-clap) &key id plugin-info-clap)
  (when (and id (not plugin-info-clap))
    (setf plugin-info-clap
          (loop for plugin-info in (plugin-info-load-all)
                  thereis (and (equal id (.id plugin-info))
                               plugin-info))))
  (when plugin-info-clap

    (setf (.host self) (utaticl.clap::make-host :module self))
    (setf (.clap-host-gui self) (utaticl.clap::make-host-gui :module self))
    (setf (.clap-host-audio-ports self) (utaticl.clap::make-host-audio-ports :module self))
    (setf (.clap-process self) (utaticl.clap::make-process))

    (multiple-value-bind (factory library)
        (utaticl.clap::get-factory (.path plugin-info-clap))

      (let ((plugin (utaticl.clap::create-plugin factory (.id plugin-info-clap) (.host self))))
        (setf (.plugin self) plugin)
        (setf (.id self) (.id plugin-info-clap))
        (setf (.name self) (.name plugin-info-clap))
        (setf (.factory self) factory)
        (setf (.library self) library)
        (cffi:foreign-funcall-pointer
         (clap:clap-plugin.init plugin) ()
         :pointer (autowrap:ptr plugin)
         :bool)
        (flet ((extension (id make writer)
                 (let ((ptr (cffi:foreign-funcall-pointer
                             (clap:clap-plugin.get-extension plugin) ()
                             :pointer (autowrap:ptr plugin)
                             :string id
                             :pointer)))
                   (unless (cffi:null-pointer-p ptr)
                     (funcall writer (funcall make :ptr ptr) self)))))
          (extension "clap.audio-ports" #'clap::make-clap-plugin-audio-ports
                     (fdefinition '(setf .ext-audio-ports)))
          (extension "clap.gui" #'clap::make-clap-plugin-gui
                     (fdefinition '(setf .ext-gui)))
          (extension "clap.latency" #'clap::make-clap-plugin-latency
                     (fdefinition '(setf .ext-latency)))
          (extension "clap.note-ports" #'clap::make-clap-plugin-note-ports
                     (fdefinition '(setf .ext-note-ports)))
          (extension "clap.state" #'clap::make-clap-plugin-state
                     (fdefinition '(setf .ext-state))))))

    (utaticl.clap::query-audio-ports self)
    (params-prepare self)
    (params-value-changed self)))

(defmethod closed ((self module-clap) was-destroyed)
  (when (.clap-window self)
    (utaticl.clap::call (clap:clap-plugin-gui.destroy (.ext-gui self))
                        :void)
    (let ((hwnd (clap:clap-window.win32 (.clap-window self))))
      (remhash (cffi:pointer-address hwnd) *hwnd-module-map*)
      (ftw:destroy-window hwnd))
    (autowrap:free (.clap-window self))
    (setf (.clap-window self) nil)))

(defmethod editor-close ((self module-clap))
  (utaticl.clap::call (clap:clap-plugin-gui.hide (.ext-gui self))
                      :bool)
  (win32::hide (clap:clap-window.win32 (.clap-window self)))
  (closed self t)
  t)

(defmethod editor-open ((self module-clap))
  (let ((ext (.ext-gui self))
        (clap-window (.clap-window self)))
    (if clap-window
        (progn
          (utaticl.clap::call (clap:clap-plugin-gui.show ext)
                              :bool)
          (win32::show (clap:clap-window.win32 clap-window))
          t)
        (when (and ext
                   (utaticl.clap::call (clap:clap-plugin-gui.is-api-supported ext)
                                       :pointer utaticl.clap::+window-api-win32+
                                       :bool nil
                                       :bool))
          (utaticl.clap::ecall (clap:clap-plugin-gui.create ext)
                               :pointer utaticl.clap::+window-api-win32+
                               :bool nil
                               :bool)
          (utaticl.clap::call (clap:clap-plugin-gui.set-scale ext)
                              :double 1d0
                              :bool)
          (let* ((resize-p (utaticl.clap::call (clap:clap-plugin-gui.can-resize ext)
                                               :bool))
                 (size (cffi:with-foreign-objects ((width :unsigned-int)
                                                   (height :unsigned-int))
                         (utaticl.clap::ecall (clap:clap-plugin-gui.get-size ext)
                                              :pointer width
                                              :pointer height
                                              :bool)
                         (@ (cffi:mem-ref width :unsigned-int)
                            (cffi:mem-ref height :unsigned-int))))
                 (hwnd (win32::make-window (.x size) (.y size) resize-p))
                 (clap-window (autowrap:alloc 'clap:clap-window-t)))
            (setf (gethash (cffi:pointer-address hwnd) *hwnd-module-map*)
                  self)
            (setf (.clap-window self) clap-window)
            (setf (clap:clap-window.api clap-window) utaticl.clap::+window-api-win32+)
            (setf (clap:clap-window.win32 clap-window) hwnd)
            (utaticl.clap::ecall (clap:clap-plugin-gui.set-parent ext)
                                :pointer (autowrap:ptr clap-window)
                                :bool)
            (utaticl.clap::call (clap:clap-plugin-gui.show ext)
                                :bool)
            t)))))

(defmethod on-resize ((self module-clap) width height)
  "called from wnd-proc wm-size"
  ;; TODO
  )

(defun plugin-scan-clap (&optional (dir "c:\\Program Files\\Common Files\\CLAP"))
  (loop for %path in (directory (merge-pathnames "**/*.clap" dir))
        for path = (namestring %path)
        for file-write-date = (file-write-date path)
        unless (uiop:directory-pathname-p path)
          nconc (multiple-value-bind (factory library) (utaticl.clap::get-factory path)
                  (unwind-protect
                       (loop for index below (cffi:foreign-funcall-pointer
                                              (clap:clap-plugin-factory.get-plugin-count factory)
                                              ()
                                              :pointer (autowrap:ptr factory)
                                              :unsigned-int)
                             for descriptor = (clap::make-clap-plugin-descriptor
                                               :ptr (cffi:foreign-funcall-pointer
                                                     (clap:clap-plugin-factory.get-plugin-descriptor factory)
                                                     ()
                                                     :pointer (autowrap:ptr factory)
                                                     :unsigned-int index
                                                     :pointer))
                             collect (make-instance 'plugin-info-clap
                                                    :descriptor descriptor
                                                    :path path
                                                    :file-write-date file-write-date))
                    (cffi:foreign-funcall "FreeLibrary" :pointer library :int)))))
;;(mapc #'describe (plugin-scan-clap))

(defmethod prepare ((self module-clap))
  (prepare (.clap-process self)))

(defmethod process ((self module-clap))
  (let* ((plugin (.plugin self))
         (clap-process (.clap-process self)))

    (apply-from clap-process *process-data*)

    (case (utaticl.clap::call (clap:clap-plugin.process plugin)
                              :pointer (autowrap:ptr clap-process)
                              :int)
      (#.clap:+clap-process-error+
       (report "failed clap process")))))

(defmethod request-callback ((self module-clap))
  (cmd-add (.project self)              ;callback なので *project* は使えない
           'cmd-request-callback
           :module self))

(defmethod request-callback-run ((self module-clap))
  (utaticl.clap::call (clap:clap-plugin.on-main-thread (.plugin self))))

(defmethod request-resize ((self module-clap) width height)
  (when (.editor-open-p self)
    (ftw:set-window-pos (clap:clap-window.win32 (.clap-window self))
                        nil 0 0 width height
                        '(:no-zorder :no-move)))
  t)

(defmethod resize-hints-changed ((self module-clap))
  (autowrap:with-alloc (hints 'clap:clap-gui-resize-hints-t)
    (utaticl.clap::call (clap:clap-plugin-gui.get-resize-hints (.ext-gui self))
                        :pointer (autowrap:ptr hints)
                        :bool)
    ;; TODO なにするの？
    ))

(defmethod start ((self module-clap))
  (unless (.start-p self)
    (utaticl.clap::ecall (clap:clap-plugin.activate (.plugin self))
                        :double (.sample-rate *config*)
                        :unsigned-int (.frames-per-buffer *config*)
                        :unsigned-int (.frames-per-buffer *config*)
                        :bool)

    (utaticl.clap::ecall (clap:clap-plugin.start-processing (.plugin self))
                        :bool)

    ;; どこかでレイテンシーの計算が必要かな

    (call-next-method)))

(defmethod stop ((self module-clap))
  (when (.start-p self)
    (utaticl.clap::call (clap:clap-plugin.stop-processing (.plugin self))
                        :void)
    (utaticl.clap::call (clap:clap-plugin.deactivate (.plugin self))
                        :void)
    (call-next-method)))

(defmethod terminate ((self module-clap) &key)
  (closed self t)
  (utaticl.clap::call (clap:clap-plugin.destroy (.plugin self))
                      :void)
  (terminate (.clap-process self))
  (terminate (.clap-host-audio-ports self))
  (terminate (.clap-host-gui self))
  (autowrap:free (.host self))

  (let* ((clap-entry (cffi:foreign-funcall "GetProcAddress"
                                           :pointer (.library self)
                                           :string "clap_entry"
                                           :pointer))
         (clap-entry (clap::make-clap-plugin-entry :ptr clap-entry))
         (deinit (clap:clap-plugin-entry.deinit clap-entry)))
    (cffi:foreign-funcall-pointer deinit () :void))

  (cffi:foreign-funcall "FreeLibrary" :pointer (.library self) :int))


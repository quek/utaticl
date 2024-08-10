(in-package :dgw)

(defmethod initialize-instance :after ((sceen-matrix sceen-matrix) &key)
  (sceen-add sceen-matrix (make-instance 'sceen)))

(defmethod clip-playing ((sceen-matrix sceen-matrix) (lane lane))
  (loop for sceen in (.sceens sceen-matrix)
        for clip = (gethash lane (.clips sceen))
          thereis (and clip (.play-p clip) clip)))

(defmethod enqueue ((sceen-matrix sceen-matrix) (clip clip))
  (push clip (.queue sceen-matrix))
  (unless (.play-p (.project sceen-matrix))
    (setf (.play-p (.project sceen-matrix)) t)))

(defmethod handle-shortcut ((sceen-matrix sceen-matrix))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-a+)
    (setf (.clips-selected sceen-matrix)
          (loop for sceen in (.sceens sceen-matrix)
                nconc (loop for clip being the hash-value in (.clips sceen)
                            collect clip))))
  (defshortcut (ig:+im-gui-key-delete+)
    (when (.clips-selected sceen-matrix)
      (cmd-add (.project sceen-matrix) 'cmd-clips-delete
               :clips (.clips-selected sceen-matrix)
               :execute-after (lambda (cmd)
                                (declare (ignore cmd))
                                (setf (.clips-selected sceen-matrix) nil)))))
  (shortcut-common (.project sceen-matrix)))

(defmethod .offset-x ((sceen-matrix sceen-matrix))
  (.offset-x (.arrangement (.project sceen-matrix))))

(defmethod (setf .play-p) (value (sceen-matrix sceen-matrix))
  (unless value
    (loop for sceen in (.sceens sceen-matrix)
          do (setf (.play-p sceen) nil))
    (setf (.queue sceen-matrix) nil)))

(defmethod prepare-event ((sceen-matrix sceen-matrix) start end loop-p offset-samples)
  (loop for clip in (nreverse (.queue sceen-matrix))
        for lane = (.lane clip)
        for clip-playing = (clip-playing sceen-matrix lane)
        if clip-playing
          collect (setf (.clip-next clip-playing) clip)
        do (setf (.will-start clip) t))
  (setf (.queue sceen-matrix) nil)

  (loop for sceen in (.sceens sceen-matrix)
        do (prepare-event sceen start end loop-p offset-samples)))

(defmethod render ((sceen-matrix sceen-matrix))
  (ig:with-styles ((ig:+im-gui-style-var-item-spacing+ (@ .0 .0)))
    (ig:with-begin ("##sceen-matrix" :flags ig:+im-gui-window-flags-no-scrollbar+)
      (ig:with-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
        (loop for y = .0 then (+ y (.height sceen))
              for sceen in (.sceens sceen-matrix)
              do (render-sceen sceen-matrix sceen y)
              finally (render-sceen-add-button sceen-matrix y)))
      (handle-shortcut sceen-matrix))))

(defmethod render-sceen ((sceen-matrix sceen-matrix) (sceen sceen) y)
  (ig:with-id (sceen)
    (ig:set-cursor-pos (@ .0 y))
    (with-renaming (sceen (.sceen-renaming sceen-matrix) (.offset-x sceen-matrix))
      (ig:with-button-color ((.color sceen)) (ig:button (.name sceen))))
    (render-sceen-track sceen-matrix sceen (.master-track (.project sceen-matrix))
                        (.offset-x sceen-matrix)
                        y)))

(defmethod render-sceen-add-button ((sceen-matrix sceen-matrix) y)
  (ig:set-cursor-pos (@ .0 y))
  (when (ig:button "+" (@ (.offset-x sceen-matrix) .0))
    ;; TODO commnad にする
    (sceen-add sceen-matrix (make-instance 'sceen))))

(defmethod render-sceen-track ((sceen-matrix sceen-matrix) (sceen sceen) (track track) x y)
  (ig:with-id (track)
    (let* ((lane (car (.lanes track)))
           (clip (gethash lane (.clips sceen)))
           (pos-local (@ x y)))
      (ig:set-cursor-pos pos-local)
      (if clip
          (with-renaming  (clip (.clip-renaming sceen-matrix) (.width lane))
            (when (member clip (.clips-selected sceen-matrix))
              (let* ((pos1 (@+ pos-local (ig:get-window-pos)))
                     (pos2 (@+ pos1 (@ (.width lane) (.height sceen))))
                     (draw-list (ig:get-window-draw-list)))
                (ig:add-rect-filled draw-list pos1 pos2
                                    (color #xa0 #xff #xa0 #x20))))
            (ig:with-button-color ((cond ((or (.will-start clip)
                                              (.will-stop clip))
                                          (if (interval-p 0.3)
                                              (.color clip)
                                              (color* (.color clip) 0.7)))
                                         (t (.color clip))))
              (when (ig:button (format nil "~:[▶~;■~]" (.play-p clip)))
                (if (.play-p clip)
                    (setf (.will-stop clip) t)
                    (enqueue sceen-matrix clip)))
              (ig:same-line)
              (when (ig:with-styles ((ig:+im-gui-col-text+
                                      (if (.play-p clip)
                                          (color #xff #x00 #xff #xff)
                                          (color #xff #xff #xff #xff))))
                      (ig:button (.name clip)))
                (unless (key-ctrl-p)
                  (setf (.clips-selected sceen-matrix) nil))
                (push clip (.clips-selected sceen-matrix))
                (edit clip))
              (ig:with-drag-drop-source ()
                (ig:set-drag-drop-payload +dd-clips+)
                (ig:text (.name clip)))
              (ig:with-drag-drop-target
                (when (ig:accept-drag-drop-payload +dd-clips+)
                  ;; TODO
                  )))
            (ig:with-popup-context-item ()
              (when (ig:menu-item "Rename")
                (setf (.clip-renaming sceen-matrix) clip))))
          (progn
            (when (ig:button "+")
              (cmd-add *project* 'cmd-clip-add
                       :clip (make-instance 'clip-note) :lane lane
                       :sceen sceen
                       :lane lane))
            (ig:with-drag-drop-target
              (when (ig:accept-drag-drop-payload +dd-clips+)
                (cmd-add *project* 'cmd-clip-add
                         :clip (copy (car (.clips-selected (.arrangement *project*))))
                         :sceen sceen
                         :lane lane))))))
    (incf x (.width track))
    (when (.tracks-show-p track)
      (loop for each-track in (.tracks track)
            do (setf x (render-sceen-track sceen-matrix sceen each-track x y))))
    x))

(defmethod sceen-add ((sceen-matrix sceen-matrix) (sceen sceen) &key before)
  (setf (.sceen-matrix sceen) sceen-matrix)
  (if before
      (labels ((f (xs)
                 (if (endp xs)
                     nil
                     (if (eq (car xs) before)
                         (psetf (car xs) sceen
                                (cdr xs) (cons (car xs) (cdr xs)))
                         (f (cdr xs))))))
        (f (.sceens sceen-matrix)))
      (setf (.sceens sceen-matrix)
            (append (.sceens sceen-matrix) (list sceen)))))


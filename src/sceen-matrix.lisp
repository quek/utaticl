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

(defmethod handle-drag-end ((sceen-matrix sceen-matrix)
                            drag-mode
                            (key-ctrl-p (eql t))
                            sceen)
  "sceen-matrix 内、または arrangement からのコピー"
  (multiple-value-bind (sceen lane)
      (world-pos-to-sceen-lane sceen-matrix *mouse-pos*)
    (cmd-add *project* 'cmd-clips-d&d-copy
             :clips (loop for (clip sceen-start lane-start clip-from)
                            being the hash-value in (.clips-dragging sceen-matrix)
                          ;; arrangement は clip-add 済なので合わせる
                          do (clip-add sceen clip :lane lane)
                          collect clip))))

(defmethod handle-drag-end ((sceen-matrix sceen-matrix)
                            drag-mode
                            (key-ctrl-p (eql nil))
                            sceen)
  "sceen-matrix 内、または arrangement からの移動"
  (let ((map (make-hash-table)))
    (loop for (clip sceen-start lane-start clip-from)
            being the hash-value in (.clips-dragging sceen-matrix)
              using (hash-key (sceen lane))
          do (setf (gethash clip-from map)
                   (list clip sceen lane)))
    (multiple-value-bind (times-to sceens-to lanes-to)
        (loop for clip in *dd-srcs*
              for (clip-to sceen lane) = (gethash clip map)
              collect 0 into times-to
              collect sceen into sceens-to
              collect lane into lanes-to
              finally (return (values times-to sceens-to lanes-to)))
      (print sceens-to)
      (cmd-add (.project sceen-matrix) 'cmd-clips-d&d-move
               :clips *dd-srcs*
               :times-to times-to
               :sceens-to sceens-to
               :lanes-to lanes-to))))

(defmethod handle-drag-end :after ((sceen-matrix sceen-matrix) drag-mode key-ctrl-p sceen)
  (setf *dd-at* nil)
  (setf *dd-srcs* nil)
  (clrhash (.clips-dragging sceen-matrix)))

(defmethod handle-drag-start ((sceen-matrix sceen-matrix))
  (cond ((and *dd-srcs* (ig:data-type-p (ig:get-drag-drop-payload) +dd-clips+)
              (null (.sceen *dd-at*)))
         (handle-dragging-intern sceen-matrix))
        ((member (.clip-at-mouse sceen-matrix) (.clips-selected sceen-matrix))
         (loop for clip in (.clips-selected sceen-matrix)
               do (setf (gethash (list (.sceen clip) (.lane clip))
                                 (.clips-dragging sceen-matrix))
                        (list (copy clip) (.sceen clip) (.lane clip) clip)))
         (setf *dd-at* (.clip-at-mouse sceen-matrix))
         (setf *dd-srcs* (.clips-selected sceen-matrix))
         (multiple-value-bind (sceen lane)
             (world-pos-to-sceen-lane sceen-matrix *mouse-pos*)
           (declare (ignore sceen))
           (setf (.drag-offset-sceen sceen-matrix) 0)
           (setf (.drag-offset-lane sceen-matrix)
                 (diff (.lane *dd-at*) lane))))
        (t                              ;範囲選択
         (setf (.range-selecting-mode sceen-matrix) :clip)
         (setf (.range-selecting-pos1 sceen-matrix) *mouse-pos*))))

(defmethod handle-dragging ((sceen-matrix sceen-matrix))
  (if (ig:is-mouse-down ig:+im-gui-mouse-button-left+)
      (handle-dragging-move sceen-matrix)
      (handle-drag-end sceen-matrix :move (key-ctrl-p)
                       (and *dd-srcs* (.sceen (car *dd-srcs*))))))

(defmethod handle-dragging-intern ((sceen-matrix sceen-matrix))
  (loop for clip in *dd-srcs*
        with sceen = (car (.sceens sceen-matrix))
        do (setf (gethash (list (.sceen clip) (.lane clip))
                          (.clips-dragging sceen-matrix))
                 (list (copy clip) sceen (.lane clip) clip)))
  (multiple-value-bind (sceen lane)
      (world-pos-to-sceen-lane sceen-matrix *mouse-pos*)
    (declare (ignore sceen))
    (setf (.drag-offset-sceen sceen-matrix) 0)
    (setf (.drag-offset-lane sceen-matrix)
          (diff (.lane *dd-at*) lane))))

(defmethod handle-dragging-move ((sceen-matrix sceen-matrix))
  (multiple-value-bind (sceen lane)
      (world-pos-to-sceen-lane sceen-matrix *mouse-pos*)
    (let ((sceen-delta (diff sceen (or (.sceen *dd-at*)
                                       (car (.sceens sceen-matrix)))))
          (lane-delta (diff lane (.lane *dd-at*))))
      (unless (and (zerop sceen-delta) (zerop lane-delta)
                   (.sceen *dd-at*))
        (loop with map = (make-hash-table :test 'equal)
              for value being the hash-value in (.clips-dragging sceen-matrix)
              for (clip sceen-start lane-start clip-from) = value
              for sceen = (relative-at sceen-start sceen-delta)
              for lane = (relative-at lane-start lane-delta)
              unless (and (eq (.sceen clip) sceen)
                          (eq (.lane clip) lane))
                do (remhash (list sceen-start lane-start)
                            (.clips-dragging sceen-matrix))
                   (setf (gethash (list sceen lane) map) value)
              finally (setf (.clips-dragging sceen-matrix) map))))))

(defmethod handle-mouse ((sceen-matrix sceen-matrix))
  (if (can-handle-mouse-p sceen-matrix)
      (cond #+TODO
            ((dragging-extern-p)
             (handle-dragging-extern sceen-matrix))
            #+TODO
            ((.dragging-source-extern sceen-matrix)
             (handle-dragging-extern-drop sceen-matrix))
            ((plusp (hash-table-count (.clips-dragging sceen-matrix)))
             (handle-dragging sceen-matrix))
            ((ig:is-mouse-dragging ig:+im-gui-mouse-button-left+)
             (handle-drag-start sceen-matrix)))))

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

  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-t+)
    (cmd-add (.project sceen-matrix) 'cmd-track-add
             :track-id-parent (.neko-id (.master-track (.project sceen-matrix)))
             :execute-after (lambda (cmd)
                              (let ((track (find-neko (.track-id-new cmd))))
                                (unselect-all-tracks (.project sceen-matrix))
                                (setf (.select-p track) t)))))

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
  (setf (.clip-at-mouse sceen-matrix) nil)
  (ig:with-styles ((ig:+im-gui-style-var-item-spacing+ (@ .0 .0)))
    (ig:with-begin ("##sceen-matrix" :flags ig:+im-gui-window-flags-no-scrollbar+)
      (ig:with-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
        (loop for y = .0 then (+ y (.height sceen))
              for sceen in (.sceens sceen-matrix)
              do (render-sceen sceen-matrix sceen y)
              finally (render-sceen-add-button sceen-matrix y)))
      (handle-mouse sceen-matrix)
      (handle-shortcut sceen-matrix))))

(defmethod render-sceen ((sceen-matrix sceen-matrix) (sceen sceen) y)
  (ig:with-id (sceen)
    (ig:set-cursor-pos (@ .0 y))
    (with-renaming (sceen (.sceen-renaming sceen-matrix) (.offset-x sceen-matrix))
      (ig:with-button-color ((.color sceen))
        (ig:button (.name sceen))))
    (render-sceen-track sceen-matrix sceen (.master-track (.project sceen-matrix))
                        (.offset-x sceen-matrix)
                        y)))

(defmethod render-sceen-add-button ((sceen-matrix sceen-matrix) y)
  (ig:set-cursor-pos (@ .0 y))
  (ig:set-next-item-shortcut (logior ig:+im-gui-mod-shift+ ig:+im-gui-key-s+))
  (when (ig:button "+" (@ (.offset-x sceen-matrix) .0))
    ;; TODO commnad にする
    (sceen-add sceen-matrix (make-instance 'sceen))))

(defmethod render-sceen-track ((sceen-matrix sceen-matrix) (sceen sceen) (track track) x y)
  (ig:with-id (track)
    (let* ((lane (car (.lanes track)))
           (clip (or (car (gethash (list sceen lane)
                                   (.clips-dragging sceen-matrix)))
                     (gethash lane (.clips sceen))))
           (pos-local (@ x y)))
      (ig:set-cursor-pos pos-local)
      (if clip
          (with-renaming  (clip (.clip-renaming sceen-matrix) (.width lane))
            (when (member clip (.clips-selected sceen-matrix))
              (let* ((pos1 (@+ pos-local (ig:get-window-pos)))
                     (pos2 (@+ pos1 (@ (.width lane) (.height sceen))))
                     (draw-list (ig:get-window-draw-list)))
                (ig:add-rect-filled draw-list pos1 pos2
                                    (color #xa0 #xff #xa0 #x20))
                (when (contain-p (ig:get-mouse-pos) pos1 pos2)
                  (setf (.clip-at-mouse sceen-matrix) clip))))
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
                (unless (member clip (.clips-selected sceen-matrix))
                  (unless (key-ctrl-p)
                    (setf (.clips-selected sceen-matrix) nil))
                  (push clip (.clips-selected sceen-matrix)))
                (setf *dd-at* clip)
                (setf *dd-srcs* (.clips-selected sceen-matrix))
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
                       :clip (make-instance 'clip-note :color (.color lane))
                       :sceen sceen
                       :lane lane))
            #+nil
            (ig:with-drag-drop-target
              (when (ig:accept-drag-drop-payload +dd-clips+)
                (drag-drop-clips sceen-matrix sceen lane (key-ctrl-p)))))))
    (incf x (.width track))
    (when (.tracks-show-p track)
      (loop for each-track in (.tracks track)
            do (setf x (render-sceen-track sceen-matrix sceen each-track x y))))
    x))

(defmethod drag-drop-clips ((sceen-matrix sceen-matrix)
                            (sceen sceen)
                            (lane lane)
                            (key-ctrl-p (eql t)))
  (let* ((lane-delta (diff (.lane *dd-at*) lane))
         (clips (loop for x in *dd-srcs*
                      for clip = (copy x)
                      do (setf (.lane clip)
                               (relative-at (.lane clip) lane-delta))
                         (setf (.sceen clip) sceen)
                      collect clip)))
    (cmd-add *project* 'cmd-clips-add :clips clips)))

(defmethod drag-drop-clips ((sceen-matrix sceen-matrix)
                            (sceen sceen)
                            (lane lane)
                            (key-ctrl-p (eql nil)))
  (cmd-add *project* 'cmd-clips-d&d-move-from-arrangement-to-sceen-matrix
           :clips-from *dd-srcs*
           :sceen-to sceen
           :lane-from (.lane *dd-at*)
           :lane-to lane))

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

(defmethod world-pos-to-sceen-lane ((sceen-matrix sceen-matrix) pos)
  (let ((sceen (world-y-to-sceen sceen-matrix (.y pos)))
        (lane (world-x-to-lane sceen-matrix (.x pos))))
   (values sceen lane)))

(defmethod world-x-to-lane ((sceen-matrix sceen-matrix) x)
  (let ((local-x (+ (- x (.x (ig:get-window-pos)) (.offset-x sceen-matrix))
                    (ig:get-scroll-x)))
        (last-lane nil))
    (map-lanes (.project sceen-matrix)
               (lambda (lane width)
                 (incf width (+ (.width lane)))
                 (if (< local-x width)
                     (return-from world-x-to-lane lane)
                     (progn
                       (setf last-lane lane)
                       width)))
               .0)
    last-lane))

(defmethod world-y-to-sceen ((sceen-matrix sceen-matrix) y)
  (loop for sceen in (.sceens sceen-matrix)
        for heigth = (+ (.y (ig:get-window-pos)) (.height sceen))
          then (+ heigth (.height sceen))
        if (< y heigth)
          do (return-from world-y-to-sceen sceen)
        finally (return-from world-y-to-sceen sceen)))

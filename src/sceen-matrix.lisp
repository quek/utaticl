(in-package :dgw)

(defmethod initialize-instance :after ((sceen-matrix sceen-matrix) &key)
  (sceen-add sceen-matrix
             (make-instance 'sceen :name (sceen-name-new sceen-matrix))))

(defmethod .offset-x ((sceen-matrix sceen-matrix))
  (.offset-x (.arrangement (.project sceen-matrix))))

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

(defmethod render ((sceen-matrix sceen-matrix))
  (ig:with-styles ((ig:+im-gui-style-var-item-spacing+ (@ .0 .0)))
    (ig:with-begin ("##sceen-matrix" :flags ig:+im-gui-window-flags-no-scrollbar+)
      (ig:with-begin-child ("##canvas" :window-flags ig:+im-gui-window-flags-horizontal-scrollbar+)
        (loop for y = .0 then (+ y (.height sceen))
              for sceen in (.sceens sceen-matrix)
              do (render-sceen sceen-matrix sceen y)
              finally (render-sceen-add-button sceen-matrix y))))))

(defmethod render-sceen ((sceen-matrix sceen-matrix) (sceen sceen) y)
  (ig:with-id (sceen)
    (ig:set-cursor-pos (@ .0 y))
    (ig:text (.name sceen))
    (render-sceen-track sceen-matrix sceen (.master-track (.project sceen-matrix))
                        (.offset-x sceen-matrix)
                        y)))

(defmethod render-sceen-add-button ((sceen-matrix sceen-matrix) y)
  (ig:set-cursor-pos (@ .0 y))
  (when (ig:button "+" (@ (.offset-x sceen-matrix) .0))
    ;; TODO commnad にする
    (sceen-add sceen-matrix
               (make-instance 'sceen
                              :name (sceen-name-new sceen-matrix)))))

(defmethod render-sceen-track ((sceen-matrix sceen-matrix) (sceen sceen) (track track) x y)
  (ig:with-id (track)
    (ig:set-cursor-pos (@ x y))
    (let* ((lane (car (.lanes track)))
           (clip (gethash lane (.clips sceen))))
      (if clip
          (when (ig:button (format nil "▶~a" (.name clip)))
            (edit clip))
          (when (ig:button "+")
            ;; TODO command
            (clip-add sceen (make-instance 'clip-note :name "クリップ") :lane lane))))
    (incf x (.width track))
    (when (.tracks-show-p track)
      (loop for each-track in (.tracks track)
            do (setf x (render-sceen-track sceen-matrix sceen each-track x y))))
    x))

(defmethod sceen-name-new ((sceen-matrix sceen-matrix))
  (format nil "S~d"
          (1+ (loop for sceen in (.sceens sceen-matrix)
                    maximize (or (ppcre:register-groups-bind
                                     ((#'parse-integer n)) ("^S(\\d+)" (.name sceen))
                                   n)
                                 0)))))

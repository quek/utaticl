(in-package :dgw)

(defmethod initialize-instance :after ((sceen-matrix sceen-matrix) &key)
  (sceen-add sceen-matrix
             (make-instance 'sceen :name (sceen-name-new sceen-matrix))))

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
        (ig:text "Sceen MATRIX")
        (render-track sceen-matrix (.master-track (.project sceen-matrix)) 0)))))

(defmethod render-track ((sceen-matrix sceen-matrix) track group-level)
  (ig:with-id (track)
    (loop for sceen in (.sceens sceen-matrix)
          do (render-sceen sceen-matrix track sceen))))

(defmethod render-sceen ((sceen-matrix sceen-matrix) track sceen)
  (ig:with-id (sceen)
    (ig:text (.name sceen))))

(defmethod sceen-name-new ((sceen-matrix sceen-matrix))
  (format nil "S~d"
          (1+ (loop for sceen in (.sceens sceen-matrix)
                    maximize (or (ppcre:register-groups-bind
                                     ((#'parse-integer n)) ("^S(\\d+)" (.name sceen))
                                   n)
                                 0)))))

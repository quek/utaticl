(in-package :utaticl.core)

(defvar *neko-map* (make-hash-table :weakness :value :test 'equal
                                    :synchronized t))

(defun find-neko (neko-id)
  (gethash neko-id *neko-map*))

(defmethod initialize-instance :after ((self neko) &key)
  (sb-ext:with-locked-hash-table (*neko-map*)
    (unless (slot-boundp self 'neko-id)
      (loop for uid = (uid)
            if (gethash uid *neko-map*)
              do (break "same uid generated ~a" uid)
            unless (gethash uid *neko-map*)
              do (setf (slot-value self 'neko-id) uid)
                 (loop-finish)))
    (setf (gethash (slot-value self 'neko-id) *neko-map*) self)))

(defmethod copy ((self neko))
  (with-serialize-context (:copy t)
    (deserialize (with-serialize-context ()
                   (serialize self)))))

(defmethod dd-show ((self neko))
  (ig:text (.name self)))

(defmethod deserialize-slot ((self neko) (slot (eql 'neko-id)) value)
  (if (.copy *serialize-context*)
      (setf (.neko-id self) (uid))
      (call-next-method)))

(assert (let ((self (make-instance 'neko)))
          (string/= (.neko-id self)
                    (.neko-id (copy self)))))

(defun name-new (class-symbol prefix)
  (sb-ext:with-locked-hash-table (*neko-map*)
    (format
     nil (format nil "~a~~d" prefix)
     (1+
      (loop for neko being the hash-value in *neko-map*
            maximize
            (or
             (and (typep neko class-symbol)
                  (ppcre:register-groups-bind
                      ((#'parse-integer n))
                      ((format nil "^~a(\\d+)$" prefix) (.name neko))
                    n))
             0))))))

(defmethod (setf .neko-id) :around (value (self neko))
  (sb-ext:with-locked-hash-table (*neko-map*)
    (let ((neko-id-old (.neko-id self)))
      (prog1 (call-next-method)
        (remhash neko-id-old *neko-map*)
        (setf (gethash value *neko-map*) self)))))

(defmethod .neko-id ((self list))
  (mapcar #'.neko-id self))

(defmethod print-object ((self neko) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a"
            (.name self)
            (.neko-id self))))

(defmethod ig:push-id ((self neko))
  (ig:push-id (.neko-id self)))

(defmethod draw ((self neko) view &key size)
  (ig:button (.name self) size))

(defmethod render-in ((self neko) view
                      &key
                        (pos (ig:get-cursor-pos) pos-supplied-p)
                        (size (@ .0 .0))
                        (rename-p t)
                        (drag-p t)
                        (drop-p t)
                        selection
                        visible-pos
                        visible-size
                        if-at-mouse)
  (when pos-supplied-p
    (ig:set-cursor-pos pos))
  (flet ((f ()
           (let ((color (if selection
                            (color-selected (.color self) (include-p selection self))
                            (.color self))))
             (ig:with-button-color (color)
               ;; クリックしてそのままドラッグしたいので
               ;; ig:button の戻り値は使わない
               (draw self view :pos pos :size size
                                           :selection selection
                                           :visible-pos visible-pos
                                           :visible-size visible-size)
               (when (and selection (ig:is-item-hovered))
                 (mouse-handle selection self)
                 (when if-at-mouse (funcall if-at-mouse)))
               (when (and selection (include-p selection self))
                 (let* ((pos1 (local-to-world view pos))
                        (pos2 (@+ pos1 size)))
                  (ig:add-rect *draw-list*
                               pos1
                               pos2
                               (color #xff #xff #x00 #xaa)
                               :thickness 3.0))))
             (when drag-p
               (dd-start view self :src (if selection
                                              (.items selection)
                                              self)))
             (when drop-p
               (dd-drop view self)))))
    (if rename-p
        (with-renaming (self (.track-renaming view) (.x size))
          (f))
        (f))))

(defmethod serialize :around ((self neko))
  (if (gethash self (.map *serialize-context*))
      `(:ref ,(.neko-id self))
      (progn
        (setf (gethash self (.map *serialize-context*)) self)
        (call-next-method))))

(in-package :utaticl.core)

(defmethod .automation-param ((self lane))
  (let ((module (.automation-module self))
        (param-id (.automation-param-id self)))
    (if (and module param-id)
        (param module param-id)
        nil)))

(defmethod (setf .automation-param) ((param param) (self lane))
  (setf (.automation-module self) (.module param))
  (setf (.automation-param-id self) (.id param)))

(defmethod clip-add ((self lane) clip &key)
  (setf (.lane clip) self)
  (setf (.clips self)
        (sort (cons clip (.clips self))
              (lambda (x y)
                (< (.time x) (.time y))))))

(defmethod clip-delete ((self lane) clip)
  (setf (.lane clip) nil)
  (setf (.clips self)
        (delete clip (.clips self))))

(defmethod diff ((a lane) (b lane))
  (labels ((f (track lane)
             (let ((pos (position lane (.lanes track))))
               (if pos
                   (values pos t)
                   (let ((distance 0)
                         (found nil))
                     (loop for x in (.tracks track)
                           until found
                           do (multiple-value-bind (n fnd) (f x lane)
                                (incf distance n)
                                (when fnd
                                  (setf found t))))
                     (values (+ distance (length (.lanes track)))
                             found))))))
    (- (f (.master-track (.project a)) a)
       (f (.master-track (.project b)) b))))

(defmethod prepare-event ((self lane) start end loop-p offset-samples)
  (loop for clip in (.clips self)
        for clip-start = (.time clip)
        for clip-end = (+ clip-start (.duration clip))
        if (and (< clip-start end)
                (< start clip-end))
          do (prepare-event clip start end loop-p offset-samples)))

(defmethod .project ((self lane))
  (.project (.track self)))

(defmethod relative-at ((self lane) delta)
  (if (zerop delta)
      self
      (let* ((all (lane-all (.project self)))
             (pos (position self all)))
        (nth (min (max (+ pos delta) 0)
                  (1- (length all)))
             all))))

(defmethod terminate ((self lane) &key)
  (loop for clip in (.clips self)
        do (terminate clip)))

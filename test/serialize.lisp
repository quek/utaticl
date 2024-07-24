;;; export してないシンボルもテストしたいのでテスト用のパッケージは作らない
(in-package #:dgw)

(setf *config* (make-instance 'config))

(fiasco:deftest test-serialize ()
  (let* ((clip1 (make-instance 'clip-note))
         (clip2 (copy clip1)))
    (destructuring-bind (clip11 clip22)
        (with-serialize-context ()
          (deserialize
           (with-serialize-context ()
             (serialize (list clip1 clip2)))))
      (fiasco:is (eq (.seq clip11) (.seq clip22))))))

(fiasco:deftest test-serialize-sceen-matrix ()
  (let* ((project (make-instance 'project))
         (sceen (car (.sceens (.sceen-matrix project))))
         (lane (car (.lanes (.master-track project)))))
    (clip-add sceen (make-instance 'clip-note) :lane lane)
    (let ((serialized (with-serialize-context ()
                        (serialize project))))
      (let ((deserialized (with-serialize-context ()
                            (deserialize serialized))))
        (fiasco:is (equalp serialized
                           (with-serialize-context ()
                             (serialize deserialized))))))))

(fiasco:deftest test-copy-lane-clip ()
  (let* ((lane (make-instance 'lane))
         (clip (make-instance 'clip-note))
         (note (make-instance 'note :time 1.0d0 :duration 0.5d0 :key 64)))
    (note-add clip note)
    (clip-add lane clip)
    (let ((copied (with-serialize-context (:copy t)
                    (deserialize (with-serialize-context ()
                                   (print (serialize lane)))))))
      (fiasco:is (= (.key note)
                    (.key (car (.notes (car (.clips copied))))))))))



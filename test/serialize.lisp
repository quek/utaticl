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
      (print serialized)
      (let ((deserialized (with-serialize-context ()
                            (deserialize serialized))))
        (fiasco:is (equalp serialized
                           (print (with-serialize-context ()
                              (serialize deserialized)))))))))



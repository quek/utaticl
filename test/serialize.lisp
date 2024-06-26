(in-package #:dgw-test)

(deftest test-serialize ()
  (let* ((clip1 (make-instance 'dgw::clip-note))
         (clip2 (dgw::copy clip1)))
    (destructuring-bind (clip11 clip22)
        (dgw::with-serialize-context ()
          (dgw::deserialize
           (dgw::with-serialize-context ()
             (dgw::serialize (list clip1 clip2)))))
      (is (eq (dgw::.seq clip11) (dgw::.seq clip22))))))

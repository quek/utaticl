(in-package :utaticl.core)

(defun make-thread-pool ()
  (setf *thread-pool* (sb-concurrency:make-mailbox :name "UTATICL-THREAD-POOL"))
  (loop for i from 1 to (max 1 (- (win32::nphysical-cpus) 2))
        do (sb-thread:make-thread
            (lambda ()
              (loop for message = (sb-concurrency:receive-message *thread-pool* :timeout 1)
                    until (eq message :quit)
                    if message
                      do (apply (car message) (cdr message)))
              (sb-concurrency:send-message *thread-pool* :quit))
            :name (format nil "UTATICL-WORKER-~d" i))))

(defmacro with-thraed-pool (&body body)
  `(unwind-protect
        (progn
          (make-thread-pool)
          ,@body)
     (sb-concurrency:send-message *thread-pool* :quit)))

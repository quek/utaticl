(in-package :utaticl.core)

(defmethod process ((self module-builtin))
  (if (and (plusp (length (.inputs *process-data*)))
           (plusp (length (.outputs *process-data*))))
      (loop for channel-index below 2
            for input-channel = (buffer-at (car (.inputs *process-data*)) channel-index)
            for output-channel = (buffer-at (car (.outputs *process-data*)) channel-index)
            for input-const-p = (const-get (car (.inputs *process-data*)) channel-index)
            do (loop for i below (.frames-per-buffer *config*)
                     do (setf (cffi:mem-aref output-channel :float i)
                              (process-sample self (cffi:mem-aref input-channel :float i))))
               (const-set (car (.outputs *process-data*)) channel-index nil)))
  (call-next-method))

(defmethod state ((self module-builtin))
  (let ((state nil))
    (maphash (lambda (key value)
               (push (list key (.value value)) state))
             (.params self))
    state))

(defmethod (setf state) (state (self module-builtin))
  (loop for (id value) in state
        do (setf (.value (param self id)) value)))

(in-package :utaticl.core)

(defmethod param ((self module-builtin) symbol)
  (gethash symbol (.params self)))

(defmethod process ((self module-builtin))
  (if (and (plusp (length (.inputs *process-data*)))
           (plusp (length (.outputs *process-data*))))
      (loop for channel-index below 2
            for input-channel = (buffer (.inputs *process-data*) 0 channel-index)
            for output-channel = (buffer (.outputs *process-data*) 0 channel-index)
            for input-silent-p = (silence-flags (.inputs *process-data*) 0 channel-index)
            if input-silent-p
              do (setf (silence-flags (.outputs *process-data*) 0 channel-index) t)
            else
              do (loop for i below (.frames-per-buffer *config*)
                       do (setf (cffi:mem-aref output-channel :float i)
                                (process-sample self (cffi:mem-aref input-channel :float i))))
                 (setf (silence-flags (.outputs *process-data*) 0 channel-index) nil))
      (setf (silence-flags (.outputs *process-data*) 0) (1- (ash 1 2))))
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

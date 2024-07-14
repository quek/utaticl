(in-package :dgw)

(defparameter *invoke-debugger-p* t)

(defgeneric my-debugger (condition))

(defmethod my-debugger (e)
  (log4cl:log-error "Error ~a!~%~a" e
                    (with-output-to-string (out)
                      (sb-debug:print-backtrace :stream out)))
  (when *invoke-debugger-p*
    (with-simple-restart (continue "Return from here.")
      (invoke-debugger e))))

(defmacro with-debugger (&body body)
  `(handler-bind ((error #'my-debugger))
     ,@body))

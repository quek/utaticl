(in-package :utaticl.core)

(defparameter *invoke-debugger-p* t)

(defgeneric my-debugger (condition))

(defmethod my-debugger (e)
  (let ((bt (with-output-to-string (out)
              (sb-debug:print-backtrace :stream out))))
    (log4cl:log-error "Error ~a!~%~a" e bt)
    (when *invoke-debugger-p*
      (with-simple-restart (continue "Return from here.")
        (invoke-debugger e)))))

(defmacro with-debugger (&body body)
  `(handler-bind ((error #'my-debugger))
     ,@body))

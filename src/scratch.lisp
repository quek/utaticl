(in-package :dgw)

(defun scratch-main ()
  (ig::create-context (cffi:null-pointer))
  (ig::get-io)
  (loop do
    (ig::new-frame)
    (when (ig::begin "Hello")
      (ig::text "World!"))
    (ig::end))









  )

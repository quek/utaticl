(in-package :dgw)

(alexandria:define-constant +dd-tracks+ "+dd-tracks+" :test #'equal)
(alexandria:define-constant +dd-extern+ "+dd-extern+" :test #'equal)

(defun dragging-extern-p ()
  (let ((payload (ig:get-drag-drop-payload)))
    (and payload (ig:is-data-type payload +dd-extern+))))

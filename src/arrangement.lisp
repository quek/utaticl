(in-package :dgw)

(defclass arrangement ()
  ())

(defmethod render ((self arrangement) context)
  (when (ig:begin "##arrangement")
    (ig:text "Arrangement")
    (ig:text (.name (.master-track *project*))))
  (ig:end))

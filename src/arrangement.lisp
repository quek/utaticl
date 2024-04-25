(in-package :dgw)

(defmethod render ((self arrangement) context)
  (when (ig:begin "##arrangement")
    (ig:text "Arrangement")
    (ig:text (.name (.master-track *project*)))
    (when (ig:begin-child "##canvas")
      (ig:text "child"))
    (ig:end-child))
  (ig:end))


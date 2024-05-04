(in-package :dgw)

(defmethod render ((self piano-roll))
  (when
    (ig:begin "##piano-roll")
    (ig:text "ピアノロール")
    (ig:text (.name (.clip self))))
  (ig:end))

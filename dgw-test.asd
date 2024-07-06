(defsystem #:dgw-test
  :depends-on ("dgw" "fiasco")
  :serial t
  :pathname "test"
  :components
  ((:file "serialize")))

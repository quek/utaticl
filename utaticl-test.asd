(defsystem #:utaticl-test
  :depends-on ("utaticl" "fiasco")
  :serial t
  :pathname "test"
  :components
  ((:file "serialize")))

(in-package :dgw)

(defconstant +grid-bar+ 4)
(defconstant +grid-beat+ 1)
(defconstant +grid-1/8+ 1/2)
(defconstant +grid-1/16+ 1/4)
(defconstant +grid-1/32+ 1/8)
(defconstant +grid-1/64+ 1/16)
(defconstant +grid-1/128+ 1/32)
(defconstant +grid-1/256+ 1/64)
(defconstant +grid-1/512+ 1/128)
(defconstant +grid-none+ 0)

(defvar *grid-all* (list +grid-none+
                         +grid-bar+
                         +grid-beat+
                         +grid-1/8+
                         +grid-1/16+
                         +grid-1/32+
                         +grid-1/64+
                         +grid-1/128+
                         +grid-1/256+
                         +grid-1/512+))

(defun grid-name (grid)
  (cond ((= grid +grid-bar+) "Bar")
        ((= grid +grid-beat+) "Beat")
        ((= grid +grid-1/8+) "1/8")
        ((= grid +grid-1/16+) "1/16")
        ((= grid +grid-1/32+) "1/32")
        ((= grid +grid-1/64+) "1/64")
        ((= grid +grid-1/128+) "1/128")
        ((= grid +grid-1/256+) "1/256")
        ((= grid +grid-1/512+) "1/512")
        ((= grid +grid-none+) "None")))

(in-package :utaticl.core)

(defmethod add ((self selection) item)
  (push item (.items self)))

(defmethod add :after ((self selection) (item track))
  (setf (.target-track *project*) item))

(defmethod mouse-handle ((self selection) item)
  (cond ((ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
         (if (key-ctrl-p)
             (progn
               (setf (.item-clicked self) nil)
               (if (include-p self item)
                   (erase self item)
                   (add self item)))
             (if (include-p self item)
                 (setf (.item-clicked self) item)
                 (progn
                   (erase-all self)
                   (add self item)))))
        ((ig:is-mouse-released ig:+im-gui-mouse-button-left+)
         (cond ((eq (.item-clicked self) item)
                (erase-all self)
                (add self item))))))

(defmethod mouse-released ((self selection))
  (setf (.item-clicked self) nil))

(defmethod erase ((self selection) item)
  (setf (.items self)
        (remove item (.items self))))

(defmethod erase-all ((self selection))
  (setf (.items self) nil))

(defmethod include-p ((self selection) item)
  (member item (.items self)))

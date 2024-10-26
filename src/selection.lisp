(in-package :utaticl.core)

(defmethod add ((self selection) item)
  (push item (.items self)))

(defmethod mouse-handle ((self selection) item)
  (cond ((ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
         (setf (.clicked-p self) t)
         (if (key-ctrl-p)
             (if (selected-p self item)
                 (erase self item)
                 (add self item))
             (unless (selected-p self item)
               (progn
                 (erase-all self)
                 (add self item)))))
        ((ig:is-mouse-released ig:+im-gui-mouse-button-left+)
         (cond ((and (null (.clicked-p self))
                     (< 1 (length (.items self))))
                (erase self item))))))

(defmethod mouse-released ((self selection))
  (setf (.clicked-p self) nil))

(defmethod erase ((self selection) item)
  (setf (.items self)
        (remove item (.items self))))

(defmethod erase-all ((self selection))
  (setf (.items self) nil))

(defmethod include-p ((self selection) item)
  (member item (.items self)))

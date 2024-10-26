(in-package :utaticl.core)

(defmethod add ((self selection) item)
  (push item (.items self))
  ;; 役割的にここじゃない気がするけど
  ;; ここだと都合がいい
  (setf (.target *project*) item))

(defmethod mouse-handle ((self selection) item)
  (cond ((ig:is-mouse-clicked ig:+im-gui-mouse-button-left+)
         (if (key-ctrl-p)
             (progn
               (setf (.clicked-p self) t)
               (if (include-p self item)
                   (erase self item)
                   (add self item)))
             (unless (include-p self item)
               (erase-all self)
               (add self item))))
        ((ig:is-mouse-released ig:+im-gui-mouse-button-left+)
         (cond ((not (.clicked-p self))
                (erase-all self)
                (add self item))))))

(defmethod mouse-released ((self selection))
  (setf (.clicked-p self) nil))

(defmethod erase ((self selection) item)
  (setf (.items self)
        (remove item (.items self))))

(defmethod erase-all ((self selection))
  (setf (.items self) nil))

(defmethod include-p ((self selection) item)
  (member item (.items self)))

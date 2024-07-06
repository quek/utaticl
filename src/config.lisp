(in-package :dgw)

(sb-ext:defglobal *config* nil)

(defclass config-mixin ()
  ((name :initarg :name :accessor .name)))

(defmethod config-load ((self config-mixin))
  (when (probe-file (config-path self))
    (loop with slot-definitions = (sb-mop:class-direct-slots (class-of self))
          for (name value) in (with-open-file (in (config-path self))
                                (read in nil nil))
          for slot-definition = (find name slot-definitions :key #'sb-mop:slot-definition-name)
          if slot-definition
            do (funcall
                (fdefinition (car (sb-mop:slot-definition-writers slot-definition)))
                value self))))

(defmethod config-path ((self config-mixin))
  (merge-pathnames (format nil "user/config/~a" (.name self)) *working-directory*))

(defmethod config-save ((self config-mixin))
  (with-open-file (out (config-path self) :direction :output :if-exists :supersede)
    (write (loop for slot-definition in (sb-mop:class-direct-slots (class-of self))
                 collect (list (sb-mop:slot-definition-name slot-definition)
                               (funcall (fdefinition (car (sb-mop:slot-definition-readers slot-definition)))
                                        self)))
           :stream out)))

(defclass config (config-mixin)
  ((audio-device-api :initform nil :accessor .audio-device-api)
   (audio-device-name :initform nil :accessor .audio-device-name)
   (frames-per-buffer :initform 1024 :accessor .frames-per-buffer)
   (sample-rate :initform 48000d0 :accessor .sample-rate))
  (:default-initargs :name "config.lisp"))


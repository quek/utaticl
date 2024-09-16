(in-package :utaticl.core)

(defmethod plugin-info-find (id)
  (find id (plugin-info-load-all) :key #'.id :test #'equalp))

(defun plugin-info-load-all ()
  (let ((path (merge-pathnames "user/config/plugins.lisp" *working-directory*)))
    (when (probe-file path)
      (with-open-file (in path)
        (loop for sexp = (read in nil nil)
              while sexp
              collect (deserialize sexp))))))


(defmethod plugin-load ((self plugin-info-vst3))
  (let ((module (module-vst3-load (.path self))))
    (setf (.name module) (.name self))
    module))


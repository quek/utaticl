(in-package :utaticl.core)

(defmethod api ((plugin-info plugin-info))
  "builtin")

(defmethod name-with-api ((plugin-info plugin-info))
  (format nil "~a ~a" (.name plugin-info) (api plugin-info)))

(defmethod plugin-info-find (id)
  (find id (plugin-info-load-all) :key #'.id :test #'equalp))

(defun plugin-info-load-all ()
  (let ((path (merge-pathnames "user/config/plugins.lisp" *working-directory*)))
    (when (probe-file path)
      (with-open-file (in path)
        (loop for sexp = (read in nil nil)
              while sexp
              collect (deserialize sexp))))))

(in-package :dgw)

(defun @ (x y)
    (list x y))

(defun @= (x y)
  (and (.x x) (.x y)
       (.y x) (.y y)))

(defun @/= (x y)
  (not (@= x y)))

(defun @+ (&rest vec2-list)
  (labels ((add (&rest args)
             (if (endp args)
                 (list 0.0 0.0)
                 (let ((car (car args))
                       (cdr (apply #'add (cdr args))))
                   (list (+ (.x car) (.x cdr))
                         (+ (.y car) (.y cdr)))))))
    (apply #'add vec2-list)))

(defun @- (vec2 &rest vec2-list)
  (if (endp vec2-list)
      (list (- (.x vec2)) (- (.y vec2)))
      (let ((rhs (apply #'@+ vec2-list)))
        (@ (- (.x vec2) (.x rhs))
           (- (.y vec2) (.y rhs))))))

(defmethod .x ((self list))
  (car self))

(defmethod .y ((self list))
  (cadr self))

(defmethod (setf .x) (value (self list))
  (setf (car self) value))

(defmethod (setf .y) (value (self list))
  (setf (cadr self) value))

(defmacro button-toggle (label var)
  `(ig:with-button-color ((if ,var
                              (.color-button-toggle-on *theme*)
                              (.color-button-toggle-off *theme*)))
     (when (ig:button ,label)
       (setf ,var (not ,var)))))

(defun contain-p (pos pos-min pos-max)
  (and (<= (.x pos-min) (.x pos) (.x pos-max))
       (<= (.y pos-min) (.y pos) (.y pos-max))))

(defun color (r g b &optional (a #x80))
  (+ (* a #x1000000)
     (* b #x10000)
     (* g #x100)
     r))

(defun color+ (a b)
  (destructuring-bind (ar ag ab aa) (color-decode a)
    (destructuring-bind (br bg bb ba) (color-decode b)
      (color (min (max (+ ar br) 0) #xff)
             (min (max (+ ag bg) 0) #xff)
             (min (max (+ ab bb) 0) #xff)
             (min (max (+ aa ba) 0) #xff)))))

(defun color* (color rate &optional (alpha-rate 1.0))
  (destructuring-bind (r g b a) (color-decode color)
    (color (min (max (round (* r rate)) 0) #xff)
           (min (max (round (* g rate)) 0) #xff)
           (min (max (round (* b rate)) 0) #xff)
           (min (max (round (* a alpha-rate)) 0) #xff))))

(defun color-decode (c)
  (list (ldb (byte 8 0) c)
        (ldb (byte 8 8) c)
        (ldb (byte 8 16) c)
        (ldb (byte 8 24) c)))

(defun color-selected (c &optional (selected t))
  (if selected
      (color+ c (color #x30 #x30 #x30 #x30))
      c))

(defmacro defshortcut ((&rest key-chord) &body body)
  (let ((code (gensym)))
    `(let ((,code (logior ,@key-chord)))
       (ig:set-next-item-shortcut ,code)
       (ig:push-id-int ,code)
       (when (ig:button "##_" (@ ig:+flt-min+ ig:+flt-min+))
         ,@body)
       (ig:pop-id))))

(defun draw-vertical-line (pos)
  (let* ((draw-list (ig:get-window-draw-list))
         (window-pos (ig:get-window-pos))
         (window-height (ig:get-window-height))
         (scroll-x (ig:get-scroll-x))
         (p1 (@+ pos window-pos (@ (- scroll-x) 0.0)))
         (p2 (@+ p1 (@ 0.0 (- window-height
                              (c-ref (ig:get-style) ig:im-gui-style :scrollbar-size))))))
    (ig:add-line draw-list p1 p2 (.color-line *theme*))))

(defun error-handler (e)
  (log:error e)
  (log:error (with-output-to-string (out)
               (sb-debug:print-backtrace :stream out)))
  (when *invoke-debugger-p*
    (with-simple-restart (continue "Return from here.")
      (invoke-debugger e))))

(defun key-alt-p ()
  (ig:ensure-to-bool (c-ref (ig:get-io) ig:im-gui-io :key-alt)))

(defun key-ctrl-p ()
  (ig:ensure-to-bool (c-ref (ig:get-io) ig:im-gui-io :key-ctrl)))

(defun key-shift-p ()
  (ig:ensure-to-bool (c-ref (ig:get-io) ig:im-gui-io :key-shift)))

(defun shortcut-common (project)
  (defshortcut (ig:+im-gui-key-space+)
    (setf (.play-p project) (not (.play-p project))))
  (defshortcut (ig:+im-gui-key-semicolon+)
    (show (.commander project)))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-s+)
    (cmd-add project 'cmd-save))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-mod-shift+ ig:+im-gui-key-s+)
    (cmd-add project 'cmd-save-as))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-y+)
    (cmd-add project 'cmd-redo))
  (defshortcut (ig:+im-gui-mod-ctrl+ ig:+im-gui-key-z+)
    (cmd-add project 'cmd-undo)))

(defun sys-window-pos ()
  (multiple-value-list (sdl2:get-window-position (.window *app*))))

(cffi:defcfun ("SetCursorPos" sys-set-cursor-pos) :int
  (x :int)
  (y :int))

(defmacro with-renaming ((object form &optional width) &body body)
  (let (($object (gensym "OBJECT")))
    `(let ((,$object ,object))
       (if (eq ,$object ,form)
           (progn
             (ig:set-keyboard-focus-here)
             ,@(awhen width
                 `((ig:set-next-item-width ,it)))
             (when (ig:input-text "##rename" (.name ,$object)
                                  :flags (logior ig:+im-gui-input-text-flags-auto-select-all+
                                                 ig:+im-gui-input-text-flags-enter-returns-true+))
               (setf ,form nil)))
           (progn
             ,@body
             (when (ig:is-item-hovered)
               (defshortcut (ig:+im-gui-key-c+)
                 (color-window ,$object))
               (defshortcut (ig:+im-gui-key-r+)
                 (setf ,form ,$object))))))))

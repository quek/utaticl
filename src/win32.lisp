(in-package :win32)

(cffi:load-foreign-library "comdlg32.dll")
(cffi:load-foreign-library "user32.dll")
(cffi:load-foreign-library "kernel32.dll")


(cffi:defctype WORD :short)
(cffi:defctype DWORD :unsigned-long)
(cffi:defctype HBRUSH :pointer)
(cffi:defctype HWND :pointer)
(cffi:defctype HCURSOR :pointer)
(cffi:defctype HICON :pointer)
(cffi:defctype HINSTANCE :pointer)
(cffi:defctype LPCTSTR (:pointer :char))
(cffi:defctype LPCWSTR (:pointer :int16))
(cffi:defctype LPTSTR (:pointer :char))
(cffi:defctype LPARAM :unsigned-long)
(cffi:defctype LRESULT (:pointer :long-long))
(cffi:defctype PVOID :pointer)
(cffi:defctype LPVOID :pointer)
(cffi:defctype LPOFNHOOKPROC :pointer)
(cffi:defctype BOOL :int32)
(cffi:defctype UINT :unsigned-int)
(cffi:defctype WPARAM (:pointer :unsigned-long-long))

(cffi:defcstruct open-file-name
  (lStructSize DWORD)
  (hwndOwner HWND)
  (hInstance HINSTANCE)
  (lpstrFilter LPCTSTR)
  (lpstrCustomFilter LPTSTR)
  (nMaxCustFilter DWORD)
  (nFilterIndex DWORD)
  (lpstrFile LPTSTR)
  (nMaxFile DWORD)
  (lpstrFileTitle LPTSTR)
  (nMaxFileTitle DWORD)
  (lpstrInitialDir LPCTSTR)
  (lpstrTitle LPCTSTR)
  (flags DWORD)
  (nFileOffset WORD)
  (nFileExtension WORD)
  (lpstrDefExt LPCTSTR)
  (lCustData LPARAM)
  (lpfnHook LPOFNHOOKPROC)
  (lpTemplateName LPCTSTR)

  (pvReserved LPVOID)
  (dwReserved DWORD)
  (flagsEx DWORD))

(defconstant OFN_FILEMUSTEXIST #x1000)
(defconstant OFN_NOCHANGEDIR #x8)
(defconstant OFN_PATHMUSTEXIST #x800)

(cffi:defcfun ("GetOpenFileNameA" %get-open-file-name) BOOL
  (param (:pointer (:struct open-file-name))))

(cffi:defcfun ("GetSaveFileNameA" %get-save-file-name) BOOL
  (param (:pointer (:struct open-file-name))))

(defun get-open-file-name (&key (filter '("lisp" "lisp")))
  (cffi:with-foreign-objects ((param '(:struct open-file-name)))
    (loop for i below (cffi:foreign-type-size '(:struct open-file-name))
          do (setf (cffi:mem-aref param :int8) 0))
    (cffi:with-foreign-strings ((file (make-string 1024))
                                (filter (with-output-to-string (out)
                                          (format out "~a~c~{*.~a~^;~}~c"
                                                  (car filter) #\null
                                                  (cdr filter) #\null)
                                          (format out "All~c*.*~c" #\null #\null)))
                                (title "select file")) 
      (cffi:with-foreign-slots ((lStructSize hwndOwner lpstrFile nMaxFile lpstrFilter nFilterIndex
                                             lpstrFileTitle flags)
                                param (:struct open-file-name))
        (setf lStructSize (cffi:foreign-type-size '(:struct open-file-name)))
        (setf hwndOwner (cffi:null-pointer))
        (setf lpstrFile file)
        (setf nMaxFile 1024)
        (setf lpstrFilter filter)
        (setf nFilterIndex 1)
        (setf lpstrFileTitle title)
        (setf flags (logior OFN_FILEMUSTEXIST OFN_NOCHANGEDIR OFN_PATHMUSTEXIST)) 

        (if (zerop (%get-open-file-name param))
            nil
            (cffi:foreign-string-to-lisp lpstrFile :encoding :CP932))))))
;;(get-open-file-name)

(defun get-save-file-name ()
  (cffi:with-foreign-objects ((param '(:struct open-file-name)))
    (loop for i below (cffi:foreign-type-size '(:struct open-file-name))
          do (setf (cffi:mem-aref param :int8) 0))
    (cffi:with-foreign-strings ((file (make-string 1024))
                                (filter (format nil "lisp~c*.lisp~cAll~c*.*~c"
                                                #\null #\null #\null #\null))) 
      (cffi:with-foreign-slots ((lStructSize hwndOwner lpstrFile nMaxFile lpstrFilter nFilterIndex
                                             lpstrFileTitle flags)
                                param (:struct open-file-name))
        (setf lStructSize (cffi:foreign-type-size '(:struct open-file-name)))
        (setf hwndOwner (cffi:null-pointer))
        (setf lpstrFile file)
        (setf nMaxFile 1024)
        (setf lpstrFilter filter)
        (setf nFilterIndex 1)
        (setf flags (logior OFN_NOCHANGEDIR OFN_PATHMUSTEXIST)) 

        (if (zerop (%get-open-file-name param))
            nil
            (cffi:foreign-string-to-lisp lpstrFile :encoding :CP932))))))
;;(get-save-file-name)


;; typedef struct tagWNDCLASSEXW {
;;     UINT        cbSize;
;;     /* Win 3.x */
;;     UINT        style;
;;     WNDPROC     lpfnWndProc;
;;     int         cbClsExtra;
;;     int         cbWndExtra;
;;     HINSTANCE   hInstance;
;;     HICON       hIcon;
;;     HCURSOR     hCursor;
;;     HBRUSH      hbrBackground;
;;     LPCWSTR     lpszMenuName;
;;     LPCWSTR     lpszClassName;
;;     /* Win 4.0 */
;;     HICON       hIconSm;
;; } WNDCLASSEXW, *PWNDCLASSEXW, NEAR *NPWNDCLASSEXW, FAR *LPWNDCLASSEXW;
(cffi:defcstruct wnd-class-exw
  (cb-size UINT)
  (style UINT)
  (lpfn-wnd-proc :pointer)
  (cb-cls-extra :int)
  (cb-wnd-extra :int)
  (h-instance HINSTANCE)
  (h-icon HICON)
  (h-cursor HCURSOR)
  (hbr-background HBRUSH)
  (lpsz-menu-name LPCWSTR)
  (lpsz-class-name LPCWSTR)
  (h-icon-sm HICON))


(defconstant +TRUE+ 1)
(defconstant +FALSE+ 1)

(defconstant +CS-DBLCLKS+ #x0008)

(defconstant +WM-ERASEBKGND+ #x0014)
(defconstant +WM-PAINT+ #x000F)
(defconstant +WM-SIZE+ #x0005)
(defconstant +WM-DESTROY+ #x0002)

(cffi:defcfun (def-window-proc-w "DefWindowProcW") LRESULT
  (hwnd HWND)
  (msg UINT)
  (wparam WPARAM)
  (lparam LPARAM))

(cffi:defcfun (get-module-handle "GetModuleHandleW") :pointer
  (x :pointer))

(cffi:defcfun (register-class-ex "RegisterClassExW") WORD
  (x :pointer))

(cffi:defcallback wnd-proc LRESULT ((hwnd HWND)
                                    (msg UINT)
                                    (wparam WPARAM)
                                    (lparam LPARAM))
  (case msg
    (#.+WM-ERASEBKGND+ +TRUE+)          ;do not draw background
    (#.+WM-PAINT+
     ;; PAINTSTRUCT ps{};
     ;; BeginPaint(hWnd, &ps);
     ;; EndPaint(hWnd, &ps);
     +FALSE+)
    (#.+WM-SIZE+
     ;; TODO リサイズする
     +FALSE+)
    (#.+WM-DESTROY+
     ;; TODO 閉じる
     (def-window-proc-w hwnd msg wparam lparam))
    (t
     (def-window-proc-w hwnd msg wparam lparam))))


(defun make-window ()
  (cffi:with-foreign-objects  ((wnd-class '(:struct wnd-class-exw)))
    (cffi:with-foreign-slots ((cb-size style lpfn-wnd-proc cb-cls-extra cb-wnd-extra
                                       h-instance h-icon h-cursor hbr-background
                                       lpsz-menu-name lpsz-class-name h-icon-sm)
                              wnd-class (:struct wnd-class-exw))
      (setf cb-size (cffi:foreign-type-size '(:struct wnd-class-exw)))
      (setf style +CS-DBLCLKS+)
      (setf lpfn-wnd-proc (cffi:callback wnd-proc))
      (setf cb-cls-extra 0)
      (setf cb-wnd-extra 0)
      (setf h-instance (get-module-handle (cffi:null-pointer)))
      (setf h-icon (cffi:null-pointer))
      (setf h-cursor (cffi:null-pointer))
      (setf hbr-background (cffi:null-pointer))
      (setf lpsz-menu-name (cffi:null-pointer))
      (setf lpsz-class-name (sb-sys:vector-sap (sb-ext:string-to-octets "VST3 Editor" :external-format :utf16le)))
      (setf h-icon-sm (cffi:null-pointer))

      (register-class-ex wnd-class)
      ))

  )

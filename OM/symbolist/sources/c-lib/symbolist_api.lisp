

(in-package :symbolist)

(cffi:defcfun ("symbolistInit" symbolistInit) :int)
(cffi:defcfun ("symbolistExit" symbolistExit) :int)
(cffi:defcfun ("symbolistInfo" symbolistInfo) :string)
(cffi:defcfun ("symbolistNewWindow" symbolistNewWindow) :pointer)
(cffi:defcfun ("symbolistNewWindowWithSymbols" symbolistNewWindowWithSymbols) :pointer (n-symbols :int) (bundle-array :pointer))
(cffi:defcfun ("symbolistWindowToFront" symbolistWindowToFront) :void (win :pointer))

(cffi:defcfun ("symbolistRegisterCloseCallback" symbolistRegisterCloseCallback) :void (win :pointer) (callback :pointer))
(cffi:defcfun ("symbolistRegisterUpdateCallback" symbolistRegisterUpdateCallback) :void (win :pointer) (callback :pointer))

(cffi::defcallback symbolist-close-callback :void ((win :pointer))
  (handler-bind ((error #'(lambda (e) (print (format nil "ERROR IN SYMBOLIST CLOSE CALLBACK: ~% ~A" e)))))
    (symbolist-handle-close-callback win)))

(cffi::defcallback symbolist-update-callback :void ((win :pointer) (bundle_array :pointer))
  (handler-bind ((error #'(lambda (e) (print (format nil "ERROR IN SYMBOLIST UPDATE CALLBACK: ~% ~A" e)))))
    (symbolist-handle-update-callback win bundle_array)))


;; call this to enable callbacks
(defun symbolist-register-callbacks (win)
  (symbolistRegisterCloseCallback win (cffi::get-callback 'symbolist-close-callback))
  (symbolistRegisterUpdateCallback win (cffi::get-callback 'symbolist-update-callback)))

;;; to be redefined
(defun symbolist-handle-close-callback (win-ptr) 
  (declare (ignore win-ptr))
  (print "symbolist close callback undefined"))

;;; to be redefined
(defun symbolist-handle-update-callback (win-ptr bundle-array-ptr) 
  (declare (ignore win-ptr bundle-array-ptr))
  (print "symbolist update callback undefined"))

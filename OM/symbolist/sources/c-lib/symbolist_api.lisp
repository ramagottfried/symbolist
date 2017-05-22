

(in-package :symbolist)

(cffi:defcfun ("symbolistInit" symbolistInit) :int)
(cffi:defcfun ("symbolistExit" symbolistExit) :int)
(cffi:defcfun ("symbolistInfo" symbolistInfo) :string)
(cffi:defcfun ("symbolistNewWindow" symbolistNewWindow) :pointer)
;;(cffi:defcfun ("symbolistNewWindowWithSymbols" symbolistNewWindowWithSymbols) :pointer (n-symbols :int) (bundle-array :pointer))
(cffi:defcfun ("symbolistCloseWindow" symbolistCloseWindow) :void (s-comp :pointer))
(cffi:defcfun ("symbolistWindowToFront" symbolistWindowToFront) :void (s-comp :pointer))
(cffi:defcfun ("symbolistWindowSetName" symbolistWindowSetName) :void (s-comp :pointer) (name :string))
(cffi:defcfun ("symbolistSetSymbols" symbolistSetSymbols) :void (s-comp :pointer) (n-symbols :int) (bundle-array :pointer))
(cffi:defcfun ("symbolistGetNumSymbols" symbolistGetNumSymbols) :int (s-comp :pointer))
(cffi:defcfun ("symbolistGetSymbol" symbolistGetSymbol) :pointer (s-comp :pointer) (n :int))
(cffi:defcfun ("symbolistRegisterCloseCallback" symbolistRegisterCloseCallback) :void (s-comp :pointer) (callback :pointer))
(cffi:defcfun ("symbolistRegisterUpdateCallback" symbolistRegisterUpdateCallback) :void (s-comp :pointer) (callback :pointer))

(cffi::defcallback symbolist-close-callback :void ((s-comp :pointer))
  (handler-bind ((error #'(lambda (e) (print (format nil "ERROR IN SYMBOLIST CLOSE CALLBACK: ~% ~A" e)))))
    (symbolist-handle-close-callback s-comp)))

(cffi::defcallback symbolist-update-callback :void ((s-comp :pointer) (n :int))
  (handler-bind ((error #'(lambda (e) (print (format nil "ERROR IN SYMBOLIST UPDATE CALLBACK: ~% ~A" e)))))
    (symbolist-handle-update-callback s-comp n)))


;; call this to enable callbacks
(defun symbolist-register-callbacks (s-comp)
  (symbolistRegisterCloseCallback s-comp (cffi::get-callback 'symbolist-close-callback))
  (symbolistRegisterUpdateCallback s-comp (cffi::get-callback 'symbolist-update-callback)))

;;; to be redefined
(defun symbolist-handle-close-callback (s-comp-ptr) 
  (declare (ignore s-comp-ptr))
  (print "symbolist close callback undefined"))

;;; to be redefined
(defun symbolist-handle-update-callback (s-comp-ptr n) 
  (declare (ignore s-comp-ptr n))
  (print "symbolist update callback undefined"))

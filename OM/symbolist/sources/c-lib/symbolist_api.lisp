

(in-package :symbolist)

(cffi:defcfun ("symbolistInit" symbolistInit) :int)
(cffi:defcfun ("symbolistExit" symbolistExit) :int)
(cffi:defcfun ("symbolistInfo" symbolistInfo) :string)
(cffi:defcfun ("symbolistNewWindow" symbolistNewWindow) :pointer)
(cffi:defcfun ("symbolistNewWindowWithSymbols" symbolistNewWindowWithSymbols) :pointer (n-symbols :int) (bundle-array :pointer))


(cffi:defcfun ("symbolistRegisterCallback" symbolistRegisterCallback) :void (win :pointer) (callback :pointer))

(defun symbolist-register-callback (win)
  (symbolistRegisterCallback win (cffi::get-callback 'symbolist-callback)))

(cffi::defcallback symbolist-callback :void ((win :pointer) (bundle_array :pointer))
  (handler-bind ((error #'(lambda (e) (print (format nil "ERROR IN SYMBOLIST CALLBACK: ~% ~A" e)))))
    (symbolist-handle-callback win bundle_array)))

;;; to be redefined
(defun symbolist-handle-callback (win-ptr bundle-array-ptr) 
  (declare (ignore win-ptr bundle-array-ptr))
  (print "symbolist callback undefined"))


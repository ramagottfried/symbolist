;===============================================================
; Basic test file for the symbolis library Lisp API
;===============================================================

(in-package :cl-user)

;;; load CFFI
;(load (merge-pathnames "ffi/load-cffi" *load-pathname*))

;;; link the C library
(fli:register-module 
   "symbolist" 
   :real-name (namestring (make-pathname :directory (append (butlast (pathname-directory *load-pathname*))
                                                            #+macosx (list "Builds" "MacOSX" "build" "Debug")
    							    #+linux (list "Builds" "Linux" "build")
    							    #+windows (list "Builds" "VisualStudio2015" "Release")
    							    )
    					 :name "symbolist" 
                                         :type #+macosx "dylib" #+linux "so" #+windows "dll"))
   :connection-style :immediate)

;;; load the bindings
(load (merge-pathnames "symbolist_api.lisp" *load-pathname*))


(defun symbolist-test ()
  (symbolist::symbolistInit)
  (print (symbolist::symbolistInfo))
  (symbolist::symbolistNewWindow))

;; (symbolist-test)


;===============================================================
; Basic test file for the symbolis library Lisp API
;===============================================================

(in-package :cl-user)

;;; load CFFI
;(load (merge-pathnames "ffi/load-cffi" *load-pathname*))

;;; link the C library
(fli:register-module 
   "symbolist" 
   :real-name (namestring (make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3)
                                                            #+macosx '("Builds" "MacOSX" "build" "Debug")
    							    #+linux '("Builds" "Linux" "build")
    							    #+windows '("Builds" "VisualStudio2015" "Release")
    							    )
    					 :name "symbolist" 
                                         :type #+macosx "dylib" #+linux "so" #+windows "dll"))
   :connection-style :immediate)

(defpackage :symbolist)

;;; will require CFFI

;;; load the bindings
(load 
 (make-pathname :directory (append (butlast (pathname-directory *load-pathname*))
                                   '("sources" "c-lib"))
                :name "symbolist_api" 
                :type "lisp")
 )



(defun symbolist-test ()
  (print (symbolist::symbolistInfo))
  (symbolist::symbolistNewWindow))

;; (symbolist-test)


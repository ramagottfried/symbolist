(in-package :cl-user)


(defpackage :symbolist)

(compile&load (merge-pathnames "symbolist_api" *load-pathname*))

(push :symbolist *features*)

(defun load-symbolist-lib ()
  (let ((libpath (merge-pathnames 
                  "lib/mac/symbolist.dylib" 
                  (om::mypathname (om::find-om-library "symbolist")))))
    (when (om-fi::om-load-foreign-library
           "symbolist"
           `((:macosx ,libpath)
             (:windows ,(om-fi::om-foreign-library-pathname "symbolist.dll"))
             (t (:default "symbolist")))))
    (print (symbolist::symbolistInfo))
    ))

;;(probe-file "C:\\Program Files (x86)\\LispWorks\\omspat.dll")
;; (fli:register-module :spat :connection-style :immediate :real-name "C:\\Program Files (x86)\\LispWorks\\omspat.dll")

;; load now
(load-symbolist-lib)

;; load at OM startup
;; #+macosx(om-fi::add-foreign-loader 'load-spat-lib)


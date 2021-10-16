;;;==============================
;;; Symbolist wrapper for OM#
;;;==============================


(in-package :om)

(require-library "odot")

(defun load-symbolist-lib ()
  (let ((libpath (merge-pathnames 
                  "lib/mac/symbolist.dylib" 
                  (om::mypathname (om::find-library "symbolist")))))
    (when (om-fi::om-load-foreign-library
           "symbolist"
           `((:macosx ,libpath)
             (:windows ,(om-fi::om-foreign-library-pathname "symbolist.dll"))
             (t (:default "symbolist")))))
    (push :symbolist *features*)
    ))

;;(probe-file "C:\\Program Files (x86)\\LispWorks\\omspat.dll")
;; (fli:register-module :spat :connection-style :immediate :real-name "C:\\Program Files (x86)\\LispWorks\\omspat.dll")

;; load now
(load-symbolist-lib)

(print (symbolist::symbolistInfo))


;; load at OM startup
;; #+macosx(om-fi::add-foreign-loader 'load-spat-lib)


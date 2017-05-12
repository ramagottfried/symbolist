
;;;===================================================
;;;
;;; symbolist 
;;; symbolic notation framework
;;;
;;; LIBRARY MAIN FILE
;;; Author: Jean Bresson, RAma Gottfried
;;;
;;;===================================================

(in-package :om)


(require-om-package "osc")

(load (om-relative-path '("sources" "c-lib") "load-symbolist"))
(compile&load (om-relative-path '("sources") "sym-score"))

(om::set-library-packages 
 '((nil nil (sym-score) nil)
   ))

(om::doc-library 
 1.0
 (om-print-format "
;;;================
;;; symbolist 1.0
;;;================
"))



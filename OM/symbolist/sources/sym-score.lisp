
(in-package :om)


;; test-util
(defun gen-score-symbols (n)
  (and n
       (sort 
        (loop for i from 1 to n collect 
              (make-instance 'osc-bundle 
                             :date (om-random 0.0 60000.0)
                             :messages `(("/staff" ,(om-random 1 2))
                                         ("/type" ,(nth-random '("circle" "square")))
                                         ("/x" ,(om-random 0.0 500.0))
                                         ("/y" ,(om-random 0.0 500.0))
                                         ("/size" ,(om-random 2.0 15.0)))))
        '< :key 'date)))
                                         
                                         
(defclass! sym-score (data-stream named-object schedulable-object object-with-action)  ; om-cleanup-mixin
  (;(score-pointer :accessor score-pointer :initform nil)
   (symbols :accessor symbols :initarg :symbols :initform '() :documentation "a list of symbols (OSC bundles)"))
  (:default-initargs :default-frame-type 'osc-bundle))

(defmethod play-obj? ((self sym-score)) t)

(defmethod data-stream-frames-slot ((self sym-score)) 'symbols)

(defmethod initialize-instance :after ((self sym-score) &rest args)
  ;(sym-score-free-score-pointer (score-pointer self))
  ;(setf (score-pointer self) (sym-score-set-score-pointer self))
  self)

;;; from om-cleanup-mixin
;(defmethod om-cleanup ((self sym-score))
;  (sym-score-free-score-pointer self))

(defmethod sym-score-make-score-pointer ((self sym-score))
  ;;(sym-score-free-score-pointer self)
  (let ((ptr (fli:allocate-foreign-object :type :pointer :nelems (length (symbols self)))))
    (om-print-dbg "allocate pointer ~A for ~A (~D symbols)" 
                  (list ptr self (length (symbols self))) 
                  "SYMBOLIST")
    (loop for symbol in (symbols self) 
          for i = 0 then (+ i 1) do  
          (setf (fli:dereference ptr :index i :type :pointer)
                (make-foreign-bundle-s-pointer (messages symbol) (date symbol))))
    ptr))

(defmethod sym-score-read-n-symbols-from-pointer ((self sym-score) bundle-array-ptr n)
  (setf (symbols self)
        (loop for i from 0 to (1- n) collect
              (let* ((ptr (fli:dereference bundle-array-ptr :index i :type :pointer))
                     (messages (om::decode-bundle-s-pointer-data ptr))) 
                (print messages)
                (make-instance 'osc-bundle) ; :messages messages)
                ))
        ))
                               
(defmethod sym-score-free-score-pointer ((self sym-score) ptr)
  (when ptr
    (om-print-dbg "free pointer ~A for ~A (~D symbols)" 
                  (list ptr self (length (symbols self))) 
                  "SYMBOLIST") 
    (dotimes (i (length (symbols self)))
      (odot::osc_bundle_s_deepFree (fli:dereference ptr :index i :type :pointer)))
    (fli:free-foreign-object ptr)
    ;(setf (score-pointer self) nil)
    ))


;;========================================================================
;; EDITOR 
;;========================================================================

(defmethod object-has-editor ((self sym-score)) t)
(defmethod get-editor-class ((self sym-score)) 'sym-editor)

(defclass sym-editor (OMEditor) 
  ((symbolist-window :accessor symbolist-window :initform nil)))

(defparameter *symbolist-editors* nil)

(defmethod open-editor-window ((self sym-editor))
  (if (symbolist-window self)
      (symbolist::symbolistWindowToFront (symbolist-window self))
    (let* ((sscore (object-value self))
           (ptr (sym-score-make-score-pointer sscore))
           (win (symbolist::symbolistNewWindowWithSymbols (length (symbols sscore)) ptr)))
      (setf (symbolist-window self) win)
      (push self *symbolist-editors*)
      (symbolist::symbolistWindowSetName win (editor-window-title self))
      (symbolist::symbolist-register-callbacks win)
      (sym-score-free-score-pointer sscore ptr)
      nil)))

(defmethod update-to-editor ((self sym-editor) (from t)) 
  (when (symbolist-window self)
    (let ((ptr (sym-score-make-score-pointer (object-value self))))
      (symbolist::symbolistWindowSetName (symbolist-window self) (editor-window-title self))
      (symbolist::symbolistwindowupdatesymbols (symbolist-window self) (length (symbols (object-value self))) ptr)
      )))

(defun symbolist::symbolist-handle-close-callback (win-ptr)
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-window :test 'om-pointer-equal)))
    (if ed
        (let ()
          (setf (symbolist-window ed) nil)
          (setf *symbolist-editors* (remove ed *symbolist-editors*)))
      (om-print "window-close callback : editor not found" "SYMBOLIST"))))

(defun symbolist::symbolist-handle-update-callback (win-ptr n-bundles bundle-array-ptr) 
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-window :test 'om-pointer-equal)))
    (if ed
        (let ((sscore (object-value ed)))
          (om-print-format "received ~D bundles" (list n-bundles) "SYMBOLIST") 
          ;(sym-score-free-score-pointer sscore)
          ;(setf (score-pointer sscore) bundle-array-ptr)
          (sym-score-read-n-symbols-from-pointer sscore bundle-array-ptr n-bundles)
          )
      (om-print "update callback : editor not found" "SYMBOLIST"))))

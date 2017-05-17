
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
                                         
                                         
(defclass! sym-score (data-stream om-cleanup-mixin named-object schedulable-object object-with-action)
  ((score-pointer :accessor score-pointer :initform nil)
   (symbols :accessor symbols :initarg :symbols :initform '() :documentation "a list of symbols (OSC bundles)"))
  (:default-initargs :default-frame-type 'osc-bundle))

(defmethod play-obj? ((self sym-score)) t)

(defmethod data-stream-frames-slot ((self sym-score)) 'symbols)

(defmethod initialize-instance :after ((self sym-score) &rest args)
  (sym-score-set-score-pointer self)
  self)

;;; from om-cleanup-mixin
(defmethod om-cleanup ((self sym-score))
  (sym-score-free-score-pointer self))

(defmethod sym-score-set-score-pointer ((self sym-score))
  (sym-score-free-score-pointer self)
  (setf (score-pointer self) (fli:allocate-foreign-object :type :pointer :nelems (length (symbols self))))
  (om-print-dbg "allocate pointer ~A in ~A (~D symbols)" 
                (list (score-pointer self) self (length (symbols self))) 
                "SYMBOLIST")
  (loop for symbol in (symbols self) 
        for i = 0 then (+ i 1) do  
        (setf (fli:dereference (score-pointer self) :index i :type :pointer)
              (make-foreign-bundle-s-pointer (messages symbol) (date symbol)))))

(defmethod sym-score-free-score-pointer ((self sym-score))
  (when (score-pointer self)
    (om-print-dbg "free pointer ~A in ~A (~D symbols)" 
                  (list (score-pointer self) self (length (symbols self))) 
                  "SYMBOLIST") 
    (dotimes (i (length (symbols self)))
      (odot::osc_bundle_s_deepFree (fli:dereference (score-pointer self) :index i :type :pointer)))
    (fli:free-foreign-object (score-pointer self))
    (setf (score-pointer self) nil)
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
  (if (window self)
      (symbolist::symbolistWindowToFront (window self))
    (let* ((sscore (object-value self))
           (win (symbolist::symbolistNewWindowWithSymbols (length (symbols sscore)) (score-pointer sscore))))
      (setf (window self) win)
      (push self *symbolist-editors*)
      (symbolist::symbolist-register-callbacks win)
      win)))

(defun symbolist::symbolist-handle-close-callback (win-ptr)
  (let ((ed (find win-ptr *symbolist-editors* :key 'window :test 'om-pointer-equal)))
    (if ed
        (let ()
          (setf (window ed) nil)
          (setf *symbolist-editors* (remove ed *symbolist-editors*)))
      (om-print "window-close callback : editor not found" "SYMBOLIST"))))

(defun symbolist::symbolist-handle-update-callback (win-ptr bundle-array-ptr) 
  (let ((ed (find win-ptr *symbolist-editors* :key 'window :test 'om-pointer-equal)))
    (if ed
        (let ()
          ;;; process the new data
          )
      (om-print "update callback : editor not found" "SYMBOLIST"))))

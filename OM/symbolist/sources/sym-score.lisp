
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
                                         
                                         
(defclass! sym-score (data-stream named-object schedulable-object object-with-action)
  ((symbols :accessor symbols :initarg :symbols :initform '() :documentation "a list of symbols (OSC bundles)"))
  (:default-initargs :default-frame-type 'osc-bundle))

(defmethod play-obj? ((self sym-score)) t)

(defmethod data-stream-frames-slot ((self sym-score)) 'symbols)

(defmethod sym-score-make-score-pointer ((self sym-score))
  (let ((ptr (fli:allocate-foreign-object :type :pointer :nelems (length (symbols self)))))
    (om-print-dbg "allocate pointer ~A for ~A (~D symbols)" 
                  (list ptr self (length (symbols self))) 
                  "SYMBOLIST")
    (loop for symbol in (symbols self) 
          for i = 0 then (+ i 1) do  
          (setf (fli:dereference ptr :index i :type :pointer)
                (make-foreign-bundle-s-pointer (messages symbol) (date symbol))))
    ptr))
                               
(defmethod sym-score-free-score-pointer ((self sym-score) ptr)
  (when ptr
    (om-print-dbg "free pointer ~A for ~A (~D symbols)" 
                  (list ptr self (length (symbols self))) 
                  "SYMBOLIST") 
    (dotimes (i (length (symbols self)))
      (odot::osc_bundle_s_deepFree (fli:dereference ptr :index i :type :pointer)))
    (fli:free-foreign-object ptr)))


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
      (symbolist::symbolistSetSymbols (symbolist-window self) (length (symbols (object-value self))) ptr)
      t
      )))
        

(defun symbolist::symbolist-handle-close-callback (win-ptr)
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-window :test 'om-pointer-equal)))
    (if ed
        (let ()
          (setf (symbolist-window ed) nil)
          (setf *symbolist-editors* (remove ed *symbolist-editors*)))
      (om-print "window-close callback : editor not found" "SYMBOLIST"))))

(defun symbolist::symbolist-handle-update-callback (win-ptr n) 
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-window :test 'om-pointer-equal)))
    (if ed
        (let ((sscore (object-value ed))
              (n-symbols (symbolist::symbolistGetNumSymbols win-ptr)))
          (om-print-format "received update callback : ~D" (list n) "SYMBOLIST") 
          (setf (symbols sscore)
                (loop for i from 0 to (1- n-symbols) collect
                      (let ((osc_b (symbolist::symbolistGetSymbol win-ptr i)))
                        (unwind-protect 
                            (make-instance 'osc-bundle
                                           :messages (om::decode-bundle-s-pointer-data osc_b))
                          (odot::osc_bundle_s_deepfree osc_b)))))
          )
      (om-print "update callback : editor not found" "SYMBOLIST"))))

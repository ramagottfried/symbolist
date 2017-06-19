
(in-package :om)


;; test-util
(defun gen-score-symbols (n)
  (and n
       (sort 
        (loop for i from 1 to n collect 
              (let ((d (om-random 0.0 60000.0)))
                (make-instance 'osc-bundle 
                               :date d
                               :messages `(("/staff" ,(om-random 1 2))
                                           ("/type" ,(nth-random '("circle"))) ; "square")))
                                           ("/x" ,(/ d 100.0))
                                         ("/y" ,(om-random 0.0 300.0))
                                         ("/w" ,(om-random 10.0 30.0))
                                         ("/h" ,(om-random 10.0 30.0))
                                         ("/offset" ,(float d))
                                         ("/duration" 1000.0)
                                         ))))
        '< :key 'date)))
                                         
                                         
(defclass! sym-score (data-stream named-object schedulable-object object-with-action)
  ((symbols :accessor symbols :initarg :symbols :initform '() :documentation "a list of symbols (OSC bundles)")
   (refresh-rate :accessor refresh-rate :initarg :refresh-rate :initform 100 :documentation "a refresh-rate for time in the symbolist editor (ms)"))
  (:default-initargs :default-frame-type 'osc-bundle))

(defmethod play-obj? ((self sym-score)) t)

(defmethod data-stream-frames-slot ((self sym-score)) 'symbols)


(defmethod get-action-list-for-play ((self sym-score) interval &optional parent)
  ;;; add some symboist time-updates
  (let ((ed (find self *symbolist-editors* :key 'object-value)))
    (if (and ed (symbolist-window ed))
        (sort 
         (append (call-next-method)
                 (loop for time from (car interval) to (cadr interval) by (refresh-rate self)
                       collect (let ((tt time))
                                 (list 
                                  tt
                                  #'(lambda ()
                                      (symbolist::symbolistSetTime (symbolist-window ed) tt))))))
         '< :key 'car)
      (call-next-method))))
  
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

(defvar *symbolist-editors* nil)

(defun close-all-symbolist-windows ()
  #'(lambda () (mapc '#(lambda (ed) (editor-close ed) sleep 1) *symbolist-editors*)))

(add-om-exit-action 'close-all-symbolist-windows)

(defmethod open-editor-window ((self sym-editor))
  (if (symbolist-window self)
      (symbolist::symbolistWindowToFront (symbolist-window self))
    (let* ((sscore (object-value self))
           (ptr (sym-score-make-score-pointer sscore))
           (s-editor (symbolist::symbolistNew)))
      (symbolist::symbolistsetsymbols s-editor (length (symbols sscore)) ptr)
      (symbolist::symbolistopenwindow s-editor)
      (setf (symbolist-window self) s-editor)
      (push self *symbolist-editors*)
      (symbolist::symbolistWindowSetName s-editor (editor-window-title self))
      (symbolist::symbolist-register-callbacks s-editor)
      (sym-score-free-score-pointer sscore ptr)
      nil)))

(defmethod update-to-editor ((self sym-editor) (from t)) 
  (when (symbolist-window self)
    (let ((ptr (sym-score-make-score-pointer (object-value self))))
      (symbolist::symbolistWindowSetName (symbolist-window self) (editor-window-title self))
      (symbolist::symbolistSetSymbols (symbolist-window self) (length (symbols (object-value self))) ptr)
      t
      )))
        
(defmethod editor-close ((self sym-editor))
  (symbolist::symbolistCloseWindow (symbolist-window self))
  (symbolist::symbolistFree (symbolist-window self))
  (setf (symbolist-window self) nil)
  (setf *symbolist-editors* (remove self *symbolist-editors*))
  (call-next-method)) 

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
          
          (om-print-format "received update callback for symbol: ~D" (list n) "SYMBOLIST") 
          
          (if (>= n 0)
              
              ;; update symnum no. n
              (let ((osc_b (symbolist::symbolistGetSymbol win-ptr n)))
                (unwind-protect 
                    (setf (nth n (symbols sscore))
                          (make-instance 'osc-bundle
                                         :messages (om::decode-bundle-s-pointer-data osc_b)))
                  (odot::osc_bundle_s_deepfree osc_b)))
            
              ;; else update all the symbols
              (setf (symbols sscore)
                (loop for i from 0 to (1- n-symbols) collect
                      (let ((osc_b (symbolist::symbolistGetSymbol win-ptr i)))
                        (unwind-protect 
                            (make-instance 'osc-bundle
                                           :messages (om::decode-bundle-s-pointer-data osc_b))
                          (odot::osc_bundle_s_deepfree osc_b)))))
            )
          (report-modifications ed)
          )
      (om-print "update callback : editor not found" "SYMBOLIST"))))



(defun symbolist::symbolist-handle-transport-callback (win-ptr command)
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-window :test 'om-pointer-equal)))
    (if ed
        (let ((box (object ed)))
          (if (= command 0) (stop-boxes (list box))
            (play-boxes (list box))))
    (om-print "transport callback : editor not found" "SYMBOLIST"))))



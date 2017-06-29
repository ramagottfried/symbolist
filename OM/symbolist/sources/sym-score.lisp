
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
                                         ("/time/start" ,(float d))
                                         ("/time/duration" 1000.0)
                                         ))))
        '< :key 'date)))
                                         
                                         
(defclass! sym-score (data-stream named-object schedulable-object object-with-action)
  ((symbols :accessor symbols :initarg :symbols :initform '() :documentation "a list of symbols in teh score (OSC bundles)")
   (palette-symbols :accessor palette-symbols :initform '() :documentation "a list of template symbols for the editing palette (OSC bundles)")
   (refresh-rate :accessor refresh-rate :initform 100 :documentation "a refresh-rate for time in the symbolist editor (ms)"))
  (:default-initargs :default-frame-type 'osc-bundle))

(defmethod play-obj? ((self sym-score)) t)

(defmethod data-stream-frames-slot ((self sym-score)) 'symbols)

(defmethod additional-class-attributes ((self sym-score)) '(palette-symbols refresh-rate))


(defun make-bundle-array-from-symbols (symbol-list)
  (let ((ptr (fli:allocate-foreign-object :type :pointer :nelems (length symbol-list))))
    (loop for symbol in symbol-list 
          for i = 0 then (+ i 1) do  
          (setf (fli:dereference ptr :index i :type :pointer)
                (make-foreign-bundle-s-pointer (messages symbol) (date symbol))))
    ptr))

(defun free-bundle-array-from-symbols (ptr symbol-list)
  (dotimes (i (length symbol-list))
    (odot::osc_bundle_s_deepFree (fli:dereference ptr :index i :type :pointer)))
  (fli:free-foreign-object ptr))

(defmethod sym-score-make-score-pointer ((self sym-score))
  (let ((ptr (make-bundle-array-from-symbols (symbols self))))
    (om-print-dbg "allocate pointer ~A for ~A (~D symbols)" 
                (list ptr self (length (symbols self))) 
                "SYMBOLIST")
    ptr))
                               
(defmethod sym-score-free-score-pointer ((self sym-score) ptr)
  (when ptr
    (om-print-dbg "free pointer ~A for ~A (~D symbols)" 
                  (list ptr self (length (symbols self))) 
                  "SYMBOLIST")
    (free-bundle-array-from-symbols ptr (symbols self))
    ))


;;========================================================================
;; EDITOR 
;;========================================================================

(defmethod object-has-editor ((self sym-score)) t)
(defmethod get-editor-class ((self sym-score)) 'sym-editor)

(defclass sym-editor (OMEditor) 
  ((symbolist-handler :accessor symbolist-handler :initform nil)))

(defvar *symbolist-editors* nil)

(defun close-all-symbolist-editors ()
  #'(lambda () (mapc '#(lambda (ed) (editor-close ed) sleep 1) *symbolist-editors*)))

(add-om-exit-action 'close-all-symbolist-editors)

(defmethod open-editor-window ((self sym-editor))
  (if (symbolist-handler self)
      (symbolist::symbolistWindowToFront (symbolist-handler self))
    (let* ((sscore (object-value self))
           (score-ptr (sym-score-make-score-pointer sscore))
           (s-editor (symbolist::symbolistNew)))
      
      (symbolist::symbolistsetsymbols s-editor (length (symbols sscore)) score-ptr)
      
      (when (palette-symbols sscore)
        (let ((palette-ptr (make-bundle-array-from-symbols (palette-symbols sscore))))
          (symbolist::symbolistsetpalettesymbols s-editor (length (palette-symbols sscore)) palette-ptr)
          (free-bundle-array-from-symbols palette-ptr (palette-symbols sscore))))
      
      (symbolist::symbolistopenwindow s-editor)
      (setf (symbolist-handler self) s-editor)
      (push self *symbolist-editors*)
      (symbolist::symbolistWindowSetName s-editor (editor-window-title self))
      (symbolist::symbolist-register-callbacks s-editor)
      (sym-score-free-score-pointer sscore score-ptr)
      
      nil)))


(defmethod save-palette-from-symbolist-handler ((self sym-editor))
  (when (symbolist-handler self)
    (let ((sscore (object-value self))
          (n-palette-symbols (symbolist::symbolistGetNumPaletteSymbols (symbolist-handler self))))
      (setf (palette-symbols sscore)
            (loop for i from 0 to (1- n-palette-symbols) collect
                  (let ((osc_b (symbolist::symbolistGetPaletteSymbol (symbolist-handler self) i)))
                    (unwind-protect 
                        (make-instance 'osc-bundle
                                       :messages (om::decode-bundle-s-pointer-data osc_b))
                      (odot::osc_bundle_s_deepfree osc_b)))))
      )))


(defmethod update-to-editor ((self sym-editor) (from t)) 
  (when (symbolist-handler self)
    (let ((ptr (sym-score-make-score-pointer (object-value self))))
      (symbolist::symbolistWindowSetName (symbolist-handler self) (editor-window-title self))
      (symbolist::symbolistSetSymbols (symbolist-handler self) (length (symbols (object-value self))) ptr)
      t
      )))
        
(defmethod editor-close ((self sym-editor))
  (when (symbolist-handler self)
    (symbolist::symbolistCloseWindow (symbolist-handler self))
    (symbolist::symbolistFree (symbolist-handler self))
    (setf (symbolist-handler self) nil))
  (setf *symbolist-editors* (remove self *symbolist-editors*))
  (call-next-method)) 

(defun symbolist::symbolist-handle-close-callback (win-ptr)
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-handler :test 'om-pointer-equal)))
    (if ed
        (let ()
          (save-palette-from-symbolist-handler ed)
          (symbolist::symbolistFree (symbolist-handler ed))
          (setf (symbolist-handler ed) nil)
          (setf *symbolist-editors* (remove ed *symbolist-editors*)))
      (om-print "window-close callback : editor not found" "SYMBOLIST")
      )))

(defun symbolist::symbolist-handle-update-callback (win-ptr n) 
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-handler :test 'om-pointer-equal)))
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
  (let ((ed (find win-ptr *symbolist-editors* :key 'symbolist-handler :test 'om-pointer-equal)))
    (if ed
        (let ((box (object ed)))
          (if (= command 0) (stop-boxes (list box))
            (play-boxes (list box))))
    (om-print "transport callback : editor not found" "SYMBOLIST"))))


(defmethod get-action-list-for-play ((self sym-score) interval &optional parent)
  ;;; add some symboist time-updates
  (let ((ed (find self *symbolist-editors* :key 'object-value)))
    (if (and ed (symbolist-handler ed))
        (sort 
         (append (call-next-method)
                 (loop for time from (car interval) to (cadr interval) by (refresh-rate self)
                       collect (let ((tt time))
                                 (list 
                                  tt
                                  #'(lambda ()
                                      (symbolist::symbolistSetTime (symbolist-handler ed) (ms->sec tt)))))))
         '< :key 'car)
      (call-next-method))))


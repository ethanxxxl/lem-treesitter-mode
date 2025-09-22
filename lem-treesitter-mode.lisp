
(defpackage :lem-treesitter-mode
  (:use :cl :alexandria
        :lem-treesitter-mode/languages
        :lem-treesitter-mode/filesystem
        :lem-treesitter-mode/treesitters
        :lem-treesitter-mode/buffer
        :lem-treesitter-mode/parser)
  (:export 
   #:setup
   ; re-export from filesystem.lisp
   #:*lem-cache-dir*
   #:*lem-treesitter-library*
   #:make-treesitter-syntax-parser
   #:load-treesitter))

(in-package :lem-treesitter-mode)



(lem:define-minor-mode treesitter-mode
    (:name "Treesitter"
     :hide-from-modeline nil
     :enable-hook 'enable
     :disable-hook 'disable))

;; TODO find the major mode symbolsfor these
(defvar *major-mode-parsers* '((:c_sharp ".cs")
                               (:lua)
                               (:rust)
                               (:python)
                               (:go)
                               (:java)
                               (:c)
                               (:cpp)
                               (:makefile)
                               (:ruby)))

(defun parse-buffer-cb (&optional (period-ms 500))
  (let ((buf (lem:current-buffer)))
    (lem-treesitter-mode/buffer:parse-buffer buf
                                             :language (lem:buffer-value buf :treesitter-lang))))

;; WIP
;; TODO
;;  - create parameter that has list of treesitter buffers
;;  - don't stop the timer until treesitter buffer list is empty
;;  - start timer once when first treesitter minor mode is enabled
;;  - buffers still need local variables
;;  - parse buffer callback should work on buffer local variables (it does)
;;    - but not on the current buffer
;;    - parse buffer needs to apply the parse-buffer treesitter command to each
;;      buffer that is in the treesitter buffers list
(defun enable ()
  ;; ensure cache directories are setup
  (ensure-directories-exist *lem-cache-dir* :verbose t :mode #o755)
  (ensure-directories-exist *lem-treesitter-library* :verbose t :mode #o755)
  
  ;; TODO autofill based on the major mode
  ;(lem:buffer-major-mode (lem:current-buffer)) 
  (let ((language (read-from-string (lem:prompt-for-string "enter language (sym): ")))
        (buf (lem:current-buffer)))
    ;(lem:message "language: ~S" language)
    ;(lem:message "lib: ~S" *lem-treesitter-library*)
    ;(unless (load-treesitter language)
    ;  (error "ERROR! ~A is not a recognized language!" language))

    (parse-buffer (lem:current-buffer) :language language)

    (lem:message "here!")
    (setf (lem:buffer-value buf :treesitter-lang) language)
        
    ;; TODO only make a new timer if one doesn't already exist.
    (setf (lem:buffer-value buf :treesitter-parser)
          (lem:start-timer (lem:make-timer 'parse-buffer-cb 
                                           :name "Treesitter Parser") 
                           500 :repeat t))))

(defun disable ()
  (when-let (timer (lem:buffer-value (lem:current-buffer) :treesitter-parser))
    (lem:stop-timer timer)))

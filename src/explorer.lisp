(defpackage :lem-treesitter-mode/explorer
  (:use :cl
        :lem-treesitter-mode/treesitters
        :lem-treesitter-mode/utils
        :lem-treesitter-mode/buffer
        :treesitter)
  (:local-nicknames (:ts :treesitter))
  (:export nil))

(in-package :lem-treesitter-mode/explorer)

(defun highlight-ts-node (ts-node source-buffer)
  "temporarily highlights (for 1000ms) the part of the buffer referenced by the TS-NODE"
  (let ((ts-start (treesitter:node-start-point ts-node))
        (ts-end (treesitter:node-end-point ts-node))
        overlay)
    (lem:with-point ((start (lem:buffer-point source-buffer)) 
                     (end (lem:buffer-point source-buffer)))

      (lem:move-to-line start (1+ (car ts-start)))
      (lem:move-to-column start (cdr ts-start))
      (lem:move-to-line end (1+ (car ts-end)))
      (lem:move-to-column end (cdr ts-end))

      (setf overlay (lem:make-overlay start end 'lem:region))
      (lem:start-timer (lem:make-timer (lambda ()
                                         (lem:delete-overlay overlay))
                                       :name "highlight treesitter node selection"
                                       :handle-function (lambda (err)
                                                          (declare (ignore err))
                                                          (ignore-errors
                                                            (lem:delete-overlay overlay))))
                       1000))))

(defmacro make-ts-node-button (ts-node source-buffer point)
  "generates a button that when clicked, highlights the region in the buffer associated with the node"
  (let ((text (gensym)))
    `(let ((,text (treesitter:node-type ,ts-node)))
       (lem/button:insert-button ,point ,text (lambda ()
                                                (highlight-ts-node ,ts-node ,source-buffer))))))


(defun %treesitter-explorer (ts-node source-buffer output-point)
  (when ts-node
    (lem:insert-string output-point "(")
    
    (make-ts-node-button ts-node source-buffer output-point)

    (when (< 0 (treesitter:node-child-count ts-node))
      (lem:insert-string output-point (format nil "~%")))
    
    (dolist (n (treesitter:node-children ts-node))
      (%treesitter-explorer n source-buffer output-point))
    
    (when (< 0 (treesitter:node-child-count ts-node))
      (lem:insert-string output-point (format nil "~%")))
    
    (lem:insert-string output-point (format nil ")"))))

(defun treesitter-explorer (n s)
  (let ((diagnostic (lem:make-buffer "*TSDiagnostic*"))
        (ts-node n)
        (source-buffer s))
    (lem:erase-buffer diagnostic)

    ;; it is important to specify :LEFT-INSERTING here, otherwise inserted text will
    ;; not be appended to the end of the buffer; rather it will be prepended.
    (lem:with-point ((end (lem:buffer-end-point diagnostic) :left-inserting))
      (%treesitter-explorer ts-node source-buffer end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HACKING BELOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(lem-treesitter-mode:setup)
;(lem-treesitter-mode/treesitters:load-treesitter :c_sharp)

;; make sure you open small.cs first.
;(defparameter *b* (lem:get-buffer "small.cs"))
;(lem-treesitter-mode/buffer:parse-buffer *b* :language :c_sharp)

;(defparameter *root-node* (treesitter:tree-root-node
;                           (lem:buffer-value *b* :tree)))

;(treesitter-explorer *root-node* *b*)

;(defparameter *diag* (lem:make-buffer "*TSDiagnostic*"))

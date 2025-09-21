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

  ;; lem doesn't currently update the point in between calls to insert-string,
  ;; so we need to move output-point to the end of the buffer after every call
  ;; manually.
  (when ts-node
    (lem:buffer-end output-point)
    (lem:insert-string output-point "(")
    
    (lem:buffer-end output-point)
    (make-ts-node-button ts-node source-buffer output-point)

    (when (< 0 (treesitter:node-child-count ts-node))
      (lem:buffer-end output-point)
      (lem:insert-string output-point (format nil "~%")))
    
    (dolist (n (treesitter:node-children ts-node))
      (%treesitter-explorer n source-buffer output-point))
    
    (lem:buffer-end output-point)
    (when (< 0 (treesitter:node-child-count ts-node))
      (lem:insert-string output-point (format nil "~%")))
    
    (lem:buffer-end output-point)
    (lem:insert-string output-point (format nil ")"))))

(defun treesitter-explorer (n s)
  (let ((diagnostic (lem:make-buffer "*TSDiagnostic*"))
        (ts-node n)
        (source-buffer s))
    (lem:erase-buffer diagnostic)

    (lem:with-point ((end (lem:buffer-point diagnostic)))
      (lem:buffer-end end)
      (%treesitter-explorer ts-node source-buffer end))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HACKING BELOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lem-treesitter-mode:setup)
(lem-treesitter-mode/treesitters:load-treesitter :c_sharp)

;; make sure you open small.cs first.
(defparameter *b* (lem:get-buffer "small.cs"))
(lem-treesitter-mode/buffer:parse-buffer *b* :language :c_sharp)

(defparameter *root-node* (treesitter:tree-root-node
                           (lem:buffer-value *b* :tree)))

(progn 
  (lem:insert-string (lem:current-point) "(")
  (make-ts-node-button *root-node* *b* (lem:current-point)))

(make-ts-node-button (nth 0 (treesitter:node-children *root-node*)) *b* (lem:current-point))

(make-ts-node-button (nth 1 (treesitter:node-children *root-node*)) *b* (lem:current-point))

(treesitter-explorer *root-node* *b*)

(defparameter *diag* (lem:make-buffer "*TSDiagnostic*"))
(lem:erase-buffer *diag*)

(lem:insert-string (lem:buffer-point *diag*) (format nil "blab~%"))
(lem:insert-string (lem:buffer-point *diag*) (format nil "blob~%"))

(lem:insert-string (lem:buffer-point *diag*) (format nil "test1~%"))
(lem:insert-string (lem:buffer-point *diag*) (format nil "test2~%"))
(lem:insert-string (lem:buffer-point *diag*) (format nil "test3~%"))
(lem:insert-string (lem:buffer-point *diag*) (format nil "test4~%"))
(lem:insert-string (lem:buffer-point *diag*) (format nil "test5~%"))

(step (lem:with-point ((end (lem:buffer-point *diag*)))
        (lem:buffer-end end)
        (lem:insert-string end (format nil "test1~%"))
        (lem:insert-string end (format nil "test2~%"))
        (lem:insert-string end (format nil "test3~%"))
        (lem:insert-string end (format nil "test4~%"))
        (lem:insert-string end (format nil "test5~%"))))0

(step) (lem:with-point ((end (lem:buffer-point *diag*)))
         (lem:buffer-end end)
         (loop for i from 1 to 5
               do (lem:insert-string end (format nil "test~A~%" i)))
         )

(lem:buffer-point *diag*)
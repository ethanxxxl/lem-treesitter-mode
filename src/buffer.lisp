; describes operations for manipulating buffers with a treesitter
(defpackage :lem-treesitter-mode/buffer
  (:use :cl
        :lem-treesitter-mode/treesitters)
  (:local-nicknames (:ts :treesitter))
  (:export
   #:parse-buffer
   #:buffer-ts-point
   #:node-text-at-point
   #:get-cursor-at-point
   #:node-at-point))

(in-package :lem-treesitter-mode/buffer)

; buffer is a lem buffer
(defun parse-buffer (buffer &key language)
  "Attempts to parse a buffer using the treesitter for the given language.

  This sets the :tree entry in the buffer's variables slot"
  (let* ((text (lem:buffer-text buffer)) 
         (parser (ensure-parser language))
         (tree (ts:parser-parse-string parser text)))
    (setf (lem:buffer-value buffer :tree) tree)))

(defun lem-point-to-ts-point (point)
  ; (row . column)
  (cons (1- (lem:line-number-at-point point))
        (lem:point-column point)))

(defun buffer-ts-point (buffer)
  (lem-point-to-ts-point (lem:buffer-point buffer)))

(defun get-cursor-at-point (buffer)
  (let ((cursor (ts:make-cursor (ts:tree-root-node (lem:buffer-value buffer :tree))))
        (bytepos (lem:point-bytes (lem:buffer-point buffer))))
    (ts:cursor-goto-first-child-for-byte cursor bytepos)
    cursor))

(defun node-at-point (buffer)
  "Gets the smallest named-node at the point"
  (let* ((start (lem:point-bytes (lem:buffer-point buffer)))
         (end start))
    (ts:node-descendant-for-range (ts:tree-root-node (lem:buffer-value buffer :tree))
                                  start
                                  end :named t)))

(defun node-text-at-point (buffer)
  (ts:node-text (node-at-point buffer)
                (lem:buffer-text buffer)))
; describes operations for manipulating buffers with a treesitter
(defpackage :lem-treesitter-mode/buffer
  (:use :cl
        :lem-treesitter-mode/treesitters)
  (:export
   #:parse-buffer))

(in-package :lem-treesitter-mode/buffer)

; buffer is a lem buffer
(defun parse-buffer (buffer &key language)
  (let ((text (lem:buffer-text buffer)) 
        (parser (ensure-parser language)))
    (treesitter:parser-parse-string parser text)))

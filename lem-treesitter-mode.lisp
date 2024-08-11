(defpackage #:lem-treesitter-mode
  (:use :cl 
        :lem-treesitter-mode/languages
        :lem-treesitter-mode/filesystem
        :lem-treesitter-mode/treesitters
        :lem-treesitter-mode/buffer)
  (:export 
   #:setup
   ; re-export from filesystem.lisp
   #:*lem-cache-dir*
   #:*lem-treesitter-library*))

(in-package :lem-treesitter-mode)

(defun setup ()
  (ensure-directories-exist *lem-cache-dir* :verbose t :mode #o755)
  (ensure-directories-exist *lem-treesitter-library* :verbose t :mode #o755))
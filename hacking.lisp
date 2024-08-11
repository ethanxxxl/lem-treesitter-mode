(ql:quickload :lem-treesitter-mode)
;(ql:quickload :pathname-utils)
(ql:quickload :cl-treesitter)

(in-package :lem-user)

; creates cache directories. Start with this.
(lem-treesitter-mode:setup)

; any of these should work
(lem-treesitter-mode/treesitters:load-treesitter :c_sharp)
(lem-treesitter-mode/treesitters:load-treesitter :bash)
(lem-treesitter-mode/treesitters:load-treesitter :rust)
(lem-treesitter-mode/treesitters:load-treesitter :c)

; you need to have opened "example.cs" for this to work
(defparameter *b* (lem:get-buffer "example.cs"))

; trace ts-node-delete
; (micros/trace:micros-trace 'treesitter/bindings:ts-node-delete)

; try some stuff
(treesitter:tree-root-node 
 (lem-treesitter-mode/buffer:parse-buffer *b* :language :c_sharp))

(treesitter:node-string
 (treesitter:tree-root-node 
  (lem-treesitter-mode/buffer:parse-buffer 
   *b* :language :c_sharp))))

; *tree* gets freed automatically at some point, I haven't figured this out yet

; trying to use *tree* elsewhere _will_ cause memory faults
;(defparameter *tree* (treesitter:tree-root-node 
;                      (lem-treesitter-mode/buffer:parse-buffer
;                       *b* :language :c_sharp)))
;; run this at your own peril
;(treesitter:node-string *tree*)
